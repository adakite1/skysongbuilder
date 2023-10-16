use std::{fs::File, io::{Read, Cursor, Write, Seek}, path::PathBuf, collections::{HashMap, HashSet, BTreeMap}, ops::RangeInclusive, str::FromStr, process::Command};

use bevy_reflect::Reflect;
use colored::Colorize;
use dse::{dsp::process_mono, dtype::{DSEError, SongBuilderFlags, AutoReadWrite, ReadWrite, SetSongBuilderFlags, Empty}, swdl::{SWDL, sf2::DSPOptions, SampleInfo}, smdl::{midi::open_midi, SMDL}, opinionated_translators::sf2midi::{FromMIDIOnce, TrimmedSampleDataCopy, FromSF2Once}};
use fileutils::get_file_last_modified_date_with_default;
use indexmap::IndexMap;
use midly::{Smf, Header, Format, num::{u15, u28, u24, u4, u7}, Track, TrackEvent};
use riff::{ChunkId, Chunk, ChunkContents};
use serde::{Serialize, Deserialize, Deserializer, de};
use serde_yaml::Value;
use soundfont::SoundFont2;
use symphonia::core::{io::MediaSourceStream, probe::Hint, formats::FormatOptions, meta::MetadataOptions, codecs::DecoderOptions, audio::SampleBuffer};
use uuid::Uuid;
use void::Void;
use std::i16;
use hound;

use crate::fileutils::open_file_overwrite_rw;

mod deserialize_with;
mod fileutils;

use deserialize_with::string_or_struct;

const VERSION: Option<&str> = option_env!("CARGO_PKG_VERSION");

const LOOP_START_MARKER: &str = "LoopStart";
const PLACEHOLDER_MARKER: &str = "Placeholder";

#[derive(Debug, Serialize, Deserialize)]
struct Song {
    mid: Option<PathBuf>,
    uses: Option<Vec<String>>,

    raw: Option<PathBuf>,
    loop_point: Option<u32>,

    #[serde(flatten)]
    shared: Shared,
}

trait MergeDefaults {
    fn merge_defaults(&mut self, other: &Self);
    fn finalize(&mut self);
}

fn deserialize_resample_at<'de, D>(deserializer: D) -> Result<Option<(f64, bool)>, D::Error>
where D: Deserializer<'de> {
    match Value::deserialize(deserializer)? {
        Value::String(buf) => {
            if buf.trim().starts_with("times") {
                buf.trim()[5..].trim().parse().map_err(serde::de::Error::custom).map(|x| Some((x, true)))
            } else {
                buf.trim().parse().map_err(serde::de::Error::custom).map(|x| Some((x, false)))
            }
        },
        Value::Number(num) => Ok(Some((num.as_f64().ok_or(de::Error::custom("Invalid number"))?, false))),
        _ => return Err(de::Error::custom("wrong type"))
    }
}
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct DSPConfig {
    resample_threshold: Option<f64>,
    #[serde(default)]
    #[serde(deserialize_with = "deserialize_resample_at")]
    resample_at: Option<(f64, bool)>,
    adpcm_encoder_lookahead: Option<u16>,
    adpcm_block_size: Option<usize>
}
impl Default for DSPConfig {
    fn default() -> Self {
        DSPConfig {
            resample_threshold: Some(0.0),
            resample_at: Some((1.0, true)),
            adpcm_encoder_lookahead: Some(3),
            adpcm_block_size: None
        }
    }
}
impl MergeDefaults for DSPConfig {
    fn merge_defaults(&mut self, other: &Self) {
        if self.resample_threshold.is_none() && other.resample_threshold.is_some() {
            self.resample_threshold = other.resample_threshold.clone();
        }
        if self.resample_at.is_none() && other.resample_at.is_some() {
            self.resample_at = other.resample_at.clone();
        }
        if self.adpcm_encoder_lookahead.is_none() && other.adpcm_encoder_lookahead.is_some() {
            self.adpcm_encoder_lookahead = other.adpcm_encoder_lookahead.clone();
        }
        if self.adpcm_block_size.is_none() && other.adpcm_block_size.is_some() {
            self.adpcm_block_size = other.adpcm_block_size.clone();
        }
    }
    fn finalize(&mut self) {
        self.merge_defaults(&DSPConfig::default())
    }
}
impl DSPConfig {
    pub fn resample_threshold(&self) -> &f64 {
        self.resample_threshold.as_ref().expect("Failed to obtain option 'resample_threshold'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn resample_at(&self) -> &(f64, bool) {
        self.resample_at.as_ref().expect("Failed to obtain option 'resample_at'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn adpcm_encoder_lookahead(&self) -> &u16 {
        self.adpcm_encoder_lookahead.as_ref().expect("Failed to obtain option 'adpcm_encoder_lookahead'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn adpcm_block_size(&self) -> Option<&usize> {
        self.adpcm_block_size.as_ref()
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum RawAudioExportFormats {
    Wav4BitAdpcmNDSPlanarLooped,
    MidiSf2,
}
impl RawAudioExportFormats {
    pub fn to_file_string(&self) -> String {
        match self {
            RawAudioExportFormats::Wav4BitAdpcmNDSPlanarLooped => "adpcm".to_string(),
            RawAudioExportFormats::MidiSf2 => "midsf2".to_string()
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Shared {
    // SWD/SMD export
    decoupled: Option<bool>,
    sample_rate_adjustment_curve: Option<usize>,
    pitch_adjust: Option<i64>,
    vcrange: Option<RangeInclusive<i8>>,

    // Raw audio export
    formats: Option<HashSet<RawAudioExportFormats>>,
    tpqn: Option<u16>,
    block_playback_delay_compensation: Option<usize>,
    load_next_block_after_ticks: Option<u32>,

    // General
    outputdir: Option<PathBuf>,
    stagingdir: Option<PathBuf>,
    dsp: Option<DSPConfig>,
    flags: Option<SongBuilderFlags>,
}
impl Default for Shared {
    fn default() -> Self {
        Shared {
            decoupled: Some(false),
            sample_rate_adjustment_curve: Some(1),
            pitch_adjust: Some(0),
            vcrange: Some(0..=15),

            formats: Some(HashSet::from_iter(std::iter::once(RawAudioExportFormats::Wav4BitAdpcmNDSPlanarLooped))),
            tpqn: Some(96),
            block_playback_delay_compensation: Some(0),
            load_next_block_after_ticks: Some(48),

            outputdir: Some(PathBuf::from("./out")),
            stagingdir: Some(PathBuf::from("./staging")),
            dsp: Some(DSPConfig::default()),
            flags: Some(SongBuilderFlags::empty())
        }
    }
}
impl MergeDefaults for Shared {
    fn merge_defaults(&mut self, other: &Self) {
        if self.decoupled.is_none() && other.decoupled.is_some() {
            self.decoupled = other.decoupled.clone();
        }
        if self.sample_rate_adjustment_curve.is_none() && other.sample_rate_adjustment_curve.is_some() {
            self.sample_rate_adjustment_curve = other.sample_rate_adjustment_curve.clone();
        }
        if self.pitch_adjust.is_none() && other.pitch_adjust.is_some() {
            self.pitch_adjust = other.pitch_adjust.clone();
        }
        if self.vcrange.is_none() && other.vcrange.is_some() {
            self.vcrange = other.vcrange.clone();
        }
        if self.formats.is_none() && other.formats.is_some() {
            self.formats = other.formats.clone();
        }
        if self.tpqn.is_none() && other.tpqn.is_some() {
            self.tpqn = other.tpqn.clone();
        }
        if self.block_playback_delay_compensation.is_none() && other.block_playback_delay_compensation.is_some() {
            self.block_playback_delay_compensation = other.block_playback_delay_compensation.clone();
        }
        if self.load_next_block_after_ticks.is_none() && other.load_next_block_after_ticks.is_some() {
            self.load_next_block_after_ticks = other.load_next_block_after_ticks.clone();
        }
        if self.outputdir.is_none() && other.outputdir.is_some() {
            self.outputdir = other.outputdir.clone();
        }
        if self.stagingdir.is_none() && other.stagingdir.is_some() {
            self.stagingdir = other.stagingdir.clone();
        }
        if self.dsp.is_none() && other.dsp.is_some() {
            self.dsp = other.dsp.clone();
        }
        if self.flags.is_none() && other.flags.is_some() {
            self.flags = other.flags.clone();
        }
        if let Some(dsp) = self.dsp.as_mut() {
            dsp.merge_defaults(other.dsp())
        }
    }
    fn finalize(&mut self) {
        self.merge_defaults(&Shared::default());
        if let Some(dsp) = self.dsp.as_mut() {
            dsp.finalize();
        }
    }
}
impl Shared {
    pub fn decoupled(&self) -> &bool {
        self.decoupled.as_ref().expect("Failed to obtain option 'decoupled'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn sample_rate_adjustment_curve(&self) -> &usize {
        self.sample_rate_adjustment_curve.as_ref().expect("Failed to obtain option 'sample_rate_adjustment_curve'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn pitch_adjust(&self) -> &i64 {
        self.pitch_adjust.as_ref().expect("Failed to obtain option 'pitch_adjust'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn vcrange(&self) -> &RangeInclusive<i8> {
        self.vcrange.as_ref().expect("Failed to obtain option 'vcrange'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn formats(&self) -> &HashSet<RawAudioExportFormats> {
        self.formats.as_ref().expect("Failed to obtain option 'formats'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn tpqn(&self) -> &u16 {
        self.tpqn.as_ref().expect("Failed to obtain option 'tpqn'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn block_playback_delay_compensation(&self) -> &usize {
        self.block_playback_delay_compensation.as_ref().expect("Failed to obtain option 'block_playback_delay_compensation'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn load_next_block_after_ticks(&self) -> &u32 {
        self.load_next_block_after_ticks.as_ref().expect("Failed to obtain option 'load_next_block_after_ticks'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn outputdir(&self) -> &PathBuf {
        self.outputdir.as_ref().expect("Failed to obtain option 'outputdir'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn stagingdir(&self) -> &PathBuf {
        self.stagingdir.as_ref().expect("Failed to obtain option 'stagingdir'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn dsp(&self) -> &DSPConfig {
        self.dsp.as_ref().expect("Failed to obtain option 'dsp'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn flags(&self) -> &SongBuilderFlags {
        self.flags.as_ref().expect("Failed to obtain option 'flags'! It's likely that 'finalize' was not called on the options.")
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct SongConfig {
    i: usize,
    #[serde(flatten)]
    song: Song
}

#[derive(Debug, Serialize, Deserialize)]
struct SoundfontConfig {
    soundfont: PathBuf,
}
impl FromStr for SoundfontConfig {
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(SoundfontConfig {
            soundfont: PathBuf::from(s),
        })
    }
    type Err = Void;
}

fn soundfonts_map<'de, D>(deserializer: D) -> Result<IndexMap<String, SoundfontConfig>, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    struct Wrapper(#[serde(deserialize_with = "string_or_struct")] SoundfontConfig);

    let v: IndexMap<String, Wrapper> = IndexMap::deserialize(deserializer)?;
    Ok(v.into_iter().map(|(k, Wrapper(a))| (k, a)).collect())
}

#[derive(Debug, Serialize, Deserialize)]
struct SoundtrackConfig {
    #[serde(flatten)]
    shared: Shared,
    mainbank: Option<PathBuf>,
    polyphone: Option<PathBuf>,
    #[serde(deserialize_with = "soundfonts_map")]
    soundfonts: IndexMap<String, SoundfontConfig>,
    songs: IndexMap<String, SongConfig>
}

fn main() -> Result<(), DSEError> {
    if let Ok(mut config_file) = File::open("./soundtrack.yml") {
        // Read in the configuration file
        let mut config_str = String::new();
        config_file.read_to_string(&mut config_str)?;
        let mut soundtrack_config: SoundtrackConfig = serde_yaml::from_str(&config_str).expect(&format!("{}Configuration file is not valid!", "Error: ".red()));
    
        // Finalize the configuration
        soundtrack_config.shared.finalize(); // Now all the getters should be safe to call
        for song_config in soundtrack_config.songs.values_mut() {
            song_config.song.shared.merge_defaults(&soundtrack_config.shared);
            // song_config.shared.finalize(); // Redundant
        }

        // Read in all the soundfont files
        let mut soundfonts: HashMap<String, SoundFont2> = HashMap::new();
        for (name, soundfont_config) in soundtrack_config.soundfonts.iter() {
            println!("[*] Opening soundfont {:?}", soundfont_config);
            soundfonts.insert(name.clone(), SoundFont2::load(&mut File::open(&soundfont_config.soundfont)?).map_err(|x| DSEError::SoundFontParseError(format!("{:?}", x)))?);
        }

        // =========== SMDL ===========

        let mut global_samples_used = None;
        let mut global_song_usage_trackers = None;

        for (name, song_config) in soundtrack_config.songs.iter_mut() {
            let mut process_sf2_and_midi = |mid: &PathBuf, uses: &Vec<String>, song: &Song, soundfont_configs: &IndexMap<String, SoundfontConfig>, soundfonts: &HashMap<String, SoundFont2>| -> Result<(), DSEError> {
                println!("[*] Reading MIDI file {:?}", mid);
                let smf_source = std::fs::read(&mid)?;
                let smf = open_midi(&smf_source)?;

                let mut smdl = SMDL::default();
                let (song_preset_map, mut samples_used, mut instrument_mappings_used, _) = smdl.from_midi_once(&smf,
                    get_file_last_modified_date_with_default(&mid)?,
                    name,
                    (0, 255),
                    song.shared.vcrange().clone(),
                    &soundfonts,
                    &uses)?;
                
                // Write to file
                smdl.save(&mut open_file_overwrite_rw(song.shared.outputdir().join(format!("bgm{:04}.smd", song_config.i)))?, Some(*song.shared.flags()))?;
            
                if !song.shared.decoupled() {
                    global_samples_used.get_or_insert(HashSet::new()).extend(samples_used.get_or_insert(HashSet::new()).iter().cloned());
                    global_song_usage_trackers.get_or_insert(HashMap::new()).insert(name.clone(), (song_preset_map, samples_used, instrument_mappings_used));
                } else {
                    // =========== SWDL (decoupled) ===========

                    let mut swdl = SWDL::default();
                    let mut sample_mapping_information: HashMap<String, (HashMap<u16, u16>, BTreeMap<u16, SampleInfo>)> = HashMap::new();
                    
                    for soundfont_name in uses {
                        let soundfont_config = soundfont_configs.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?;
                        let sf2 = soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?;
                        let &(sample_rate, sample_rate_relative) = song.shared.dsp().resample_at();
                        sample_mapping_information.insert(soundfont_name.clone(), swdl.trimmed_raw_sample_copy(soundfont_name,
                            &File::open(&soundfont_config.soundfont)?,
                            sf2,
                            DSPOptions {
                                resample_threshold: song.shared.dsp().resample_threshold().round() as u32,
                                sample_rate,
                                sample_rate_relative,
                                adpcm_encoder_lookahead: *song.shared.dsp().adpcm_encoder_lookahead() as i32
                            },
                            *song.shared.sample_rate_adjustment_curve(),
                            *song.shared.pitch_adjust(),
                            &samples_used.get_or_insert(HashSet::new()))?);
                    }

                    swdl.from_sf2_once(&soundfonts,
                        &uses,
                        get_file_last_modified_date_with_default(&mid)?,
                        name,
                        (0, 255),
                        song.shared.vcrange().clone(),
                        *song.shared.sample_rate_adjustment_curve(),
                        *song.shared.pitch_adjust(),
                        &song_preset_map,
                        &sample_mapping_information,
                        &instrument_mappings_used.get_or_insert(HashSet::new()),
                        &samples_used.get_or_insert(HashSet::new()))?;
                    
                    swdl.save(&mut open_file_overwrite_rw(song.shared.outputdir().join(format!("bgm{:04}.swd", song_config.i)))?, Some(*song.shared.flags()))?;
                }

                Ok(())
            };
            let mut process_raw_audio = |raw: PathBuf, loop_point: Option<u32>, song: &mut Song| -> Result<(), DSEError> {
                // Use symphonia to read in the raw audio data
                let raw_audio_file = Box::new(File::open(&raw)?);
                let mss = MediaSourceStream::new(raw_audio_file, Default::default());
                let hint = Hint::new();
                let format_opts: FormatOptions = Default::default();
                let metadata_opts: MetadataOptions = Default::default();
                let decoder_opts: DecoderOptions = Default::default();
                let probed = symphonia::default::get_probe().format(&hint, mss, &format_opts, &metadata_opts).expect(&format!("Internal Error: Error probing for the format of the raw audio file '{:?}'!", &raw));
                let mut format = probed.format;
                let track = format.default_track().expect(&format!("Internal Error: Raw audio file '{:?}' doesn't contain a single track!", &raw));
                let track_sample_rate = track.codec_params.sample_rate.expect("Internal Error: Error reading audio sample rate!");
                let mut decoder = symphonia::default::get_codecs().make(&track.codec_params, &decoder_opts).unwrap();
                let track_id = track.id;
                let mut sample_count = 0;
                let mut sample_buf = None;
                let mut left_samples: Vec<i16> = Vec::new();
                let mut right_samples: Vec<i16> = Vec::new();
                loop {
                    match format.next_packet() {
                        Ok(packet) => {
                            if packet.track_id() != track_id {
                                continue;
                            }
                            match decoder.decode(&packet) {
                                Ok(audio_buf) => {
                                    if sample_buf.is_none() {
                                        let spec = *audio_buf.spec();
                                        let duration = audio_buf.capacity() as u64;
                                        sample_buf = Some(SampleBuffer::<i16>::new(duration, spec));
                                    }
                                    if let Some(buf) = &mut sample_buf {
                                        buf.copy_planar_ref(audio_buf);
                                        sample_count += buf.samples().len();
                                        print!("\nDecoded {} samples", sample_count);
    
                                        // Store
                                        let (packet_left_samples, packet_right_samples) = buf.samples().split_at(buf.samples().len() / 2);
                                        left_samples.extend(packet_left_samples);
                                        right_samples.extend(packet_right_samples);
                                    }
                                },
                                Err(symphonia::core::errors::Error::IoError(_)) => {
                                    // The packet failed to decode due to an IO error, skip the packet.
                                    continue;
                                },
                                Err(symphonia::core::errors::Error::DecodeError(_)) => {
                                    // The packet failed to decode due to invalid data, skip the packet.
                                    continue;
                                },
                                Err(_) => break,
                            }
                        },
                        Err(symphonia::core::errors::Error::IoError(e)) => {
                            match e.kind() {
                                std::io::ErrorKind::UnexpectedEof => {
                                    // Failed to get the next packet because the end of the file has been reached. Stop here.
                                    break;
                                },
                                _ => {
                                    // For other errors, continue looping.
                                    continue;
                                },
                            }
                        },
                        Err(symphonia::core::errors::Error::DecodeError(_)) => {
                            continue;
                        },
                        Err(_) => break,
                    }
                }
                
                //TODO: This doesn't work with the playback delay compensation. Need to implement that.
                let fix_loop = |left_samples: &mut Vec<i16>, right_samples: &mut Vec<i16>, block_size: usize, loop_point: &Option<u32>| -> Option<usize> {
                    let mut loop_start_block_i = None;
                    if let Some(loop_point) = &loop_point {
                        // If we are looping, repeat the audio data from the loop point to the end of the block that that loop point belongs to and append it to the end of the audio so that the loop can start cleanly from a fresh block.
                        let loop_copy_block_start_index = *loop_point as usize;
                        loop_start_block_i = Some(*loop_point as usize / block_size + 1);
                        let loop_copy_block_end_index = (loop_start_block_i.unwrap() * block_size).min(left_samples.len());
                        left_samples.extend_from_within(loop_copy_block_start_index..loop_copy_block_end_index);
                        right_samples.extend_from_within(loop_copy_block_start_index..loop_copy_block_end_index);
                    }
                    loop_start_block_i
                };

                for format in song.shared.formats().iter() {
                    match format {
                        RawAudioExportFormats::MidiSf2 => {
                            // Hard code block size to 1 second
                            let block_size = track_sample_rate as usize;
                            // Fix looping
                            let loop_start_block_i = fix_loop(&mut left_samples, &mut right_samples, block_size, &loop_point);

                            // Split into blocks
                            let mut blocks: Vec<(&[i16], &[i16])> = Vec::new();
                            let mut block_i = 0;
                            let mut sample_i = 0;
                            // A simple function to write a 16-bit PCM mono audio file
                            fn write_wave_simple<W: Write + Seek>(file: W, track_sample_rate: u32, audio: &[i16]) {
                                let spec = hound::WavSpec {
                                    channels: 1,
                                    sample_rate: track_sample_rate,
                                    bits_per_sample: 16,
                                    sample_format: hound::SampleFormat::Int,
                                };
                                let mut writer = hound::WavWriter::new(file, spec).unwrap();
                                for &t in audio {
                                    writer.write_sample(t).unwrap();
                                }
                            }
                            loop {
                                if sample_i >= left_samples.len() { break; }
                                let left_block = &left_samples[sample_i..(sample_i + block_size + if sample_i == 0 { *song.shared.block_playback_delay_compensation() } else { 0 }).min(left_samples.len())];
                                let right_block = &right_samples[sample_i..(sample_i + block_size + if sample_i == 0 { *song.shared.block_playback_delay_compensation() } else { 0 }).min(right_samples.len())];

                                // For each sample, write to filesystem as WAVE files.
                                let left_file = open_file_overwrite_rw(
                                    soundtrack_config.shared.stagingdir().join(name.to_string()).join(format!("{}_L.wav", block_i))
                                )?;
                                write_wave_simple(&left_file, track_sample_rate, left_block);
                                let right_file = open_file_overwrite_rw(
                                    soundtrack_config.shared.stagingdir().join(name.to_string()).join(format!("{}_R.wav", block_i))
                                )?;
                                write_wave_simple(&right_file, track_sample_rate, right_block);

                                blocks.push((left_block, right_block));


                                sample_i += block_size + if sample_i == 0 { *song.shared.block_playback_delay_compensation() } else { 0 };
                                block_i += 1;
                            }

                            // Create the sfz file and midi file at the same time
                            let mut sfz_file = "
//------------------------------------------------------------------------------
// A basic sfz template
//------------------------------------------------------------------------------
<control>
default_path= // relative path of your samples

<global>
// parameters that affect the whole instrument go here.
amp_veltrack=0

// *****************************************************************************
// Your mapping starts here
// *****************************************************************************

<group> // 1

// Parameters that affect multiple regions go here

fil_type=         // One of the many filter types available
cutoff=           // freq in hertz
cutoff_onccX=     // variation in cents
resonance=        // value in db
resonance_onccX=  // variation in db

ampeg_attack=0.01
ampeg_release=0.01

trigger=attack    // or release or first or legato
loop_mode=no_loop // or loop_continuous or one_shot or loop_sustain

".to_string();
                            
                            let tpqn = *song.shared.tpqn();
                            let mut midi_file = Smf::new(Header::new(Format::SingleTrack, midly::Timing::Metrical(u15::from_int_lossy(tpqn))));
                            midi_file.tracks.push(Track::new());
                            midi_file.tracks[0].push(TrackEvent {
                                delta: u28::from_int_lossy(0),
                                kind: midly::TrackEventKind::Meta(midly::MetaMessage::Tempo(u24::from_int_lossy(250000)))
                            });
                            // PlaySample MIDI(key=n, vel=n, len!=0)
                            // ToggleLoadSamples MIDI(key=n, vel=n, len=0)
                            // ReloadSamples MIDI(key=127, vel=127, len=anything)
                            fn make_streaming_midi_event<'a>(chan: u4, key: u7, vel: u7, len: u28, shift: u28) -> [TrackEvent<'a>; 2] {
                                [TrackEvent {
                                    delta: shift,
                                    kind: midly::TrackEventKind::Midi {
                                        channel: chan,
                                        message: midly::MidiMessage::NoteOn {
                                            key,
                                            vel
                                        }
                                    }
                                },
                                TrackEvent {
                                    delta: len,
                                    kind: midly::TrackEventKind::Midi {
                                        channel: chan,
                                        message: midly::MidiMessage::NoteOff {
                                            key,
                                            vel
                                        }
                                    }
                                }]
                            }
                            fn map_chunk_i(chunk_i: usize) -> (u8, u8) {
                                let keycenter = (chunk_i / 128) as u8;
                                let velcenter = (chunk_i % 127) as u8 + 1;
                                (keycenter, velcenter)
                            }
                            // Setup some tracking variables for MIDI writing.
                            // ToggleLoadSamples the first chunk.
                            let first_chunk_i = 0;
                            let (first_keycenter, first_velcenter) = map_chunk_i(first_chunk_i);
                            let (mut unload_keycenter, mut unload_velcenter) = (first_keycenter, first_velcenter);
                            let (mut keycenter_unloaded_by_loopchunk, mut velcenter_unloaded_by_loopchunk) = (None, None);
                            let mut which_chan: u8 = 1;
                            let mut previous_note_off: Option<TrackEvent> = None;
                            for (chunk_i, (_, _)) in blocks.iter().enumerate() {
                                let (keycenter, velcenter) = map_chunk_i(chunk_i);

                                // If we are looping...
                                if let Some(loop_start_block) = loop_start_block_i {
                                    // And this block is the loop block, make sure to plant the LoopStart flag here.
                                    if loop_start_block == chunk_i {
                                        midi_file.tracks[0].push(TrackEvent {
                                            delta: u28::from_int_lossy(0),
                                            kind: midly::TrackEventKind::Meta(midly::MetaMessage::Marker(LOOP_START_MARKER.as_bytes()))
                                        });
                                    }
                                }

                                // Switch channels. If this is the loop block, write in a separate channel so that when looping two consecutive chunks at the loop point doesn't share a single channel.
                                if loop_start_block_i.is_some() && loop_start_block_i.unwrap() == chunk_i {
                                    which_chan = 3;
                                } else if which_chan == 1 {
                                    which_chan = 2;
                                } else {
                                    which_chan = 1;
                                }

                                if chunk_i == 0 {
                                    // Load samples for the first chunk immediately since no previous chunk will load it for us.
                                    midi_file.tracks[0].extend(make_streaming_midi_event(
                                        u4::from_int_lossy(0),
                                        u7::from_int_lossy(keycenter),
                                        u7::from_int_lossy(velcenter),
                                        u28::from_int_lossy(0),
                                        u28::from_int_lossy(0),
                                    ));
                                }

                                // This function writes in the events necessary to shift the buffers to be loaded forward by one.
                                let mut shift_buffer = |tracks: &mut Vec<Track>, shift: u32| {
                                    let mut shifted = false;
            
                                    // If this isn't the first chunk, then there's a chunk that needs to be unloaded.
                                    if chunk_i != 0 {
                                        shifted = true;
                                        // ToggleLoadSamples the previous chunk (unload).
                                        tracks[0].extend(make_streaming_midi_event(
                                            u4::from_int_lossy(0),
                                            u7::from_int_lossy(unload_keycenter),
                                            u7::from_int_lossy(unload_velcenter),
                                            u28::from_int_lossy(0),
                                            u28::from_int_lossy(shift),
                                        ));
                                        // If we are looping...
                                        if let Some(loop_start_block) = loop_start_block_i {
                                            // And this block is the loop block, keep a record of which note is unloaded here, since when it loops back these shouldn't be unloaded, so a preemptive toggle is necessary later on at the song's end.
                                            if loop_start_block == chunk_i {
                                                keycenter_unloaded_by_loopchunk = Some(unload_keycenter);
                                                velcenter_unloaded_by_loopchunk = Some(unload_velcenter);
                                            }
                                        }
                                    }
            
                                    // If this isn't the last chunk or we are looping, there's still a chunk ahead that needs to be loaded right now.
                                    if chunk_i != blocks.len()-1 || loop_start_block_i.is_some() {
                                        let future_chunk_i;
                                        // Determine the next chunk that needs to be loaded. 
                                        if chunk_i != blocks.len()-1 {
                                            future_chunk_i = chunk_i + 1;
                                        } else {
                                            future_chunk_i = loop_start_block_i.unwrap();
                                        }
                                        let (future_keycenter, future_velcenter) = map_chunk_i(future_chunk_i);
                                        // ToggleLoadSamples the next chunk.
                                        tracks[0].extend(make_streaming_midi_event(
                                            u4::from_int_lossy(0),
                                            u7::from_int_lossy(future_keycenter),
                                            u7::from_int_lossy(future_velcenter),
                                            u28::from_int_lossy(0),
                                            u28::from_int_lossy(if !shifted {
                                                shifted = true;
                                                shift
                                            } else { 0 }),
                                        ));
                                    }

                                    // Reload samples.
                                    tracks[0].extend(make_streaming_midi_event(
                                        u4::from_int_lossy(0),
                                        u7::from_int_lossy(127),
                                        u7::from_int_lossy(127),
                                        u28::from_int_lossy(0),
                                        u28::from_int_lossy(if !shifted {
                                            shifted = true;
                                            shift
                                        } else { 0 }),
                                    ));
                                };

                                // Add entries for the current chunk into the sfz file.
                                sfz_file += &format!("<region> trigger=attack pitch_keycenter={} lokey={} hikey={} lovel={} hivel={} pan=-100 sample={}\n",
                                    keycenter, keycenter, keycenter,
                                    velcenter, velcenter,
                                    format!("{}_L.wav", chunk_i));
                                sfz_file += &format!("<region> trigger=attack pitch_keycenter={} lokey={} hikey={} lovel={} hivel={} pan=100 sample={}\n",
                                    keycenter, keycenter, keycenter,
                                    velcenter, velcenter,
                                    format!("{}_R.wav", chunk_i));
                                
                                // Calculate note length. Always round up so as to not cut the loop short.
                                // let note_length = ((left_chunk.len() as f64 / track_sample_rate as f64) * (tpqn as f64 * 4.0)).ceil() as u32;
                                //TODO: This is again something that needs to be dealt with for looped audio files, since the last note will need to actually reflect the audio file's looping.
                                let note_length = tpqn as u32 * 4; // No need to calculate the exact note length since notes are overlapped anyways.
                                
                                let play_sample_evts: [TrackEvent; 2];
                                if chunk_i == 0 {
                                    // If this is the first chunk, we want to shift and reload the samples immediately.
                                    shift_buffer(&mut midi_file.tracks, 0);
                                    
                                    // PlaySample (Note On)
                                    play_sample_evts = make_streaming_midi_event(
                                        u4::from_int_lossy(which_chan),
                                        u7::from_int_lossy(keycenter),
                                        u7::from_int_lossy(velcenter),
                                        u28::from_int_lossy(note_length),
                                        u28::from_int_lossy(0),
                                    );

                                    midi_file.tracks[0].push(play_sample_evts[0]);
                                } else {
                                    // Otherwise, only reload the samples after the note is off on its way.
                                    // PlaySample (Note On)
                                    play_sample_evts = make_streaming_midi_event(
                                        u4::from_int_lossy(which_chan),
                                        u7::from_int_lossy(keycenter),
                                        u7::from_int_lossy(velcenter),
                                        u28::from_int_lossy(note_length - *song.shared.load_next_block_after_ticks()), // Shift by load_next_block_after_ticks to give room for the sample to start sounding.
                                        u28::from_int_lossy(0),
                                    );

                                    midi_file.tracks[0].push(play_sample_evts[0]);

                                    // Shift and reload samples once the new note is on its way and load_next_block_after_ticks ticks has passed.
                                    shift_buffer(&mut midi_file.tracks, *song.shared.load_next_block_after_ticks());
                                }

                                // PlaySample (Note Off)
                                if let Some(previous_note_off) = previous_note_off.take() {
                                    midi_file.tracks[0].push(previous_note_off);
                                } else {
                                    midi_file.tracks[0].push(TrackEvent {
                                        delta: u28::from_int_lossy(note_length),
                                        kind: midly::TrackEventKind::Meta(midly::MetaMessage::Marker(PLACEHOLDER_MARKER.as_bytes()))
                                    });
                                }
                                previous_note_off = Some(play_sample_evts[1]);

                                // Mark this chunk for unloading.
                                unload_keycenter = keycenter;
                                unload_velcenter = velcenter;

                                // If this is the last chunk, there's still a chunk that needs to be unloaded.
                                if chunk_i == blocks.len()-1 {
                                    // ToggleLoadSamples the previous chunk (unload).
                                    midi_file.tracks[0].extend(make_streaming_midi_event(
                                        u4::from_int_lossy(0),
                                        u7::from_int_lossy(unload_keycenter),
                                        u7::from_int_lossy(unload_velcenter),
                                        u28::from_int_lossy(0),
                                        u28::from_int_lossy(0),
                                    ));

                                    // Also if this is the last chunk and we are looping, there are additional things that need to be done.
                                    if let Some(_) = loop_start_block_i {
                                        // The loop block will try to unload the previous chunk's data, but in this case, the previous chunk is this last chunk. Thus it will toggle the wrong block. Here we preemptively toggle it to cancel out that error.
                                        if let (Some(keycenter_unloaded_by_loopchunk), Some(velcenter_unloaded_by_loopchunk)) = (keycenter_unloaded_by_loopchunk, velcenter_unloaded_by_loopchunk) {
                                            // Preemptively do ToggleLoadSamples on the chunk that will be incorrectly toggled by the loop starting chunk.
                                            midi_file.tracks[0].extend(make_streaming_midi_event(
                                                u4::from_int_lossy(0),
                                                u7::from_int_lossy(keycenter_unloaded_by_loopchunk),
                                                u7::from_int_lossy(velcenter_unloaded_by_loopchunk),
                                                u28::from_int_lossy(0),
                                                u28::from_int_lossy(0),
                                            ));
                                        }
                                    }
                                }
                            }

                            // Deal with the last note off
                            if let Some(previous_note_off) = previous_note_off.take() {
                                midi_file.tracks[0].push(previous_note_off);
                            }

                            // The MIDI file is now complete

                            // Write in the sfz file
                            open_file_overwrite_rw(
                                soundtrack_config.shared.stagingdir().join(name.to_string()).join("bank.sfz")
                            )?.write_all(sfz_file.as_bytes())?;
                            // Write in the midi file
                            midi_file.write_std(&mut open_file_overwrite_rw(
                                soundtrack_config.shared.stagingdir().join(name.to_string()).join("song.mid")
                            )?)?;

                            // Delete the previously converted sf2 if it exists, since Polyphone won't do anything if it does
                            let sf2_path = soundtrack_config.shared.stagingdir().join(name.to_string()).join("bank.sf2");
                            if sf2_path.exists() {
                                std::fs::remove_file(&sf2_path)?;
                            }
                            // Use Polyphone to convert the sfz into an sf2 soundfont
                            if let Some(polyphone) = soundtrack_config.polyphone.as_ref() {
                                Command::new(polyphone)
                                    .arg("-1")
                                    .arg("-i")
                                    .arg(soundtrack_config.shared.stagingdir().join(name.to_string()).join("bank.sfz"))
                                    .output().unwrap();
                                if !sf2_path.exists() {
                                    panic!("{}Polyphone didn't generate the soundfont!", "Internal Error: ".red());
                                }
                            } else {
                                panic!("{}Path to polyphone binary must be specified when exporting raw audio!", "Error: ".red())
                            }

                            // Fill in the song
                            if song.mid.is_none() {
                                song.mid = Some(soundtrack_config.shared.stagingdir().join(name.to_string()).join("song.mid"));
                            } else {
                                //TODO: Make this a feature.
                                panic!("{}TODO", "Internal Error: ".red());
                            }
                            if song.uses.is_none() {
                                let bank_path = soundtrack_config.shared.stagingdir().join(name.to_string()).join("bank.sf2");
                                let generated_sf2_id = Uuid::new_v4().to_string();

                                soundtrack_config.soundfonts.insert(generated_sf2_id.clone(), SoundfontConfig {
                                    soundfont: bank_path.clone(),
                                });
                                soundfonts.insert(generated_sf2_id.clone(), SoundFont2::load(&mut File::open(bank_path)?).map_err(|x| DSEError::SoundFontParseError(format!("{:?}", x)))?);

                                song.uses = Some(vec![generated_sf2_id.clone()]);
                            } else {
                                //TODO: Make this a feature.
                                panic!("{}TODO", "Internal Error: ".red());
                            }
                        },
                        RawAudioExportFormats::Wav4BitAdpcmNDSPlanarLooped => {
                            // Figure out the block size
                            let block_size = if let Some(adpcm_block_size) = song.shared.dsp().adpcm_block_size() {
                                *adpcm_block_size
                            } else {
                                left_samples.len()
                            };
                            // Fix looping
                            let loop_start_block_i = fix_loop(&mut left_samples, &mut right_samples, block_size, &loop_point);

                            // Open output files
                            let left_file_path;
                            let right_file_path;
                            if song.shared.formats().len() == 1 {
                                left_file_path = soundtrack_config.shared.outputdir().join(format!("bgm{:04}_left.wav", song_config.i));
                                right_file_path = soundtrack_config.shared.outputdir().join(format!("bgm{:04}_right.wav", song_config.i));
                            } else {
                                left_file_path = soundtrack_config.shared.outputdir().join(format!("bgm{:04}_left_{}.wav", song_config.i, format.to_file_string()));
                                right_file_path = soundtrack_config.shared.outputdir().join(format!("bgm{:04}_right_{}.wav", song_config.i, format.to_file_string()));
                            }
                            let mut left_file = open_file_overwrite_rw(left_file_path)?;
                            let mut right_file = open_file_overwrite_rw(right_file_path)?;

                            // Encode blocks
                            let &(sample_rate, sample_rate_relative) = song.shared.dsp().resample_at();
                            let mut new_sample_rate = if track_sample_rate as f64 > *song.shared.dsp().resample_threshold() {
                                if sample_rate_relative {
                                    if sample_rate >= 1.0 {
                                        sample_rate * (track_sample_rate as f64)
                                    } else {
                                        let mut accum = track_sample_rate as f64;
                                        while accum > *song.shared.dsp().resample_threshold() {
                                            accum *= sample_rate;
                                        }
                                        accum
                                    }
                                } else {
                                    sample_rate
                                }
                            } else {
                                track_sample_rate as f64
                            }.round(); // Round for better compatibility with the NDS's sound systems.

                            let wave_id: ChunkId = ChunkId { value: [0x57, 0x41, 0x56, 0x45] }; //WAVE
                            let fmt_id: ChunkId = ChunkId { value: [0x66, 0x6D, 0x74, 0x20] }; //fmt\0
                            let smpl_id: ChunkId = ChunkId { value: [0x73, 0x6D, 0x70, 0x6C] }; //smpl
                            let data_id: ChunkId = ChunkId { value: [0x64, 0x61, 0x74, 0x61] }; //data

                            #[derive(Debug, Clone, Default, Reflect)]
                            struct WaveAdpcmFmtChunk {
                                wFormatTag: u16,
                                nChannels: u16,
                                nSamplesPerSec: u32,
                                nAvgBytesPerSec: u32,
                                nBlockAlign: u16,
                                wBitsPerSample: u16,
                                cbSize: u16,
                                extraData: [u8; 32]
                            }
                            impl AutoReadWrite for WaveAdpcmFmtChunk {  }
                            let extraData = [0; 32];
                            let nBlockAlign: u16 = (song.shared.dsp().adpcm_block_size().map(|x| *x).unwrap_or(505) as u16 - 1) / 2 + 4;
                            let mut fmt_data = {
                                let mut data: Vec<u8> = Vec::new();
                                let mut cursor = Cursor::new(&mut data);
                                WaveAdpcmFmtChunk {
                                    wFormatTag: 0x11,
                                    nChannels: 1,
                                    nSamplesPerSec: new_sample_rate as u32,
                                    nAvgBytesPerSec: (((new_sample_rate / 2.0).ceil() as u32 - 1) | 255) + 1,
                                    nBlockAlign,
                                    wBitsPerSample: 4,
                                    cbSize: 32,
                                    extraData
                                }.write_to_file(&mut cursor)?;
                                data
                            };

                            ChunkContents::Children(
                                riff::RIFF_ID,
                                wave_id,
                                vec![
                                    ChunkContents::Data(fmt_id, fmt_data.clone()),
                                    ChunkContents::Data(data_id, process_mono(&left_samples, track_sample_rate as f64, new_sample_rate, *song.shared.dsp().adpcm_encoder_lookahead() as i32, song.shared.dsp().adpcm_block_size().cloned(), &[]).0)
                                ]
                            ).write(&mut left_file)?;
                            ChunkContents::Children(
                                riff::RIFF_ID,
                                wave_id,
                                vec![
                                    ChunkContents::Data(fmt_id, fmt_data),
                                    ChunkContents::Data(data_id, process_mono(&right_samples, track_sample_rate as f64, new_sample_rate, *song.shared.dsp().adpcm_encoder_lookahead() as i32, song.shared.dsp().adpcm_block_size().cloned(), &[]).0)
                                ]
                            ).write(&mut right_file)?;
                        }
                    }
                }

                Ok(())
            };

            if let Some(raw) = &song_config.song.raw {
                process_raw_audio(raw.clone(), song_config.song.loop_point.clone(), &mut song_config.song)?;
            }
            if let (Some(mid), Some(uses)) = (&song_config.song.mid, &song_config.song.uses) {
                process_sf2_and_midi(mid, uses, &song_config.song, &soundtrack_config.soundfonts, &soundfonts)?;
            }
        }

        // =========== MAIN BANK SWDL ===========

        if let (Some(global_samples_used), Some(mut global_song_usage_trackers)) = (global_samples_used, global_song_usage_trackers) {
            // Read in the main bank swd file
            let mut main_bank_swdl;
            if let Some(mainbank) = soundtrack_config.mainbank.as_ref() {
                main_bank_swdl = SWDL::load_path(&mainbank)?;
            } else {
                panic!("{}The `mainbank` path must be specified when running in non-decoupled mode!", "Error: ".red())
            }

            // Start patching in the SF2 files one by one into the main bank, keeping a record of how samples are mapped for each one
            let mut sample_mapping_information: HashMap<String, (HashMap<u16, u16>, BTreeMap<u16, SampleInfo>)> = HashMap::new();
            for (soundfont_name, sf2) in soundfonts.iter() {
                // If this check fails, that just means that this soundfont isn't used at all. Skip.
                println!("[*] Opening soundfont {:?} for patching", &soundtrack_config.soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?);
                let (sample_rate, sample_rate_relative) = soundtrack_config.shared.dsp().resample_at();
                sample_mapping_information.insert(soundfont_name.clone(), main_bank_swdl.trimmed_raw_sample_copy(soundfont_name,
                    &File::open(&soundtrack_config.soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?.soundfont)?,
                    sf2,
                    DSPOptions {
                        resample_threshold: soundtrack_config.shared.dsp().resample_threshold().round() as u32,
                        sample_rate: *sample_rate,
                        sample_rate_relative: *sample_rate_relative,
                        adpcm_encoder_lookahead: *soundtrack_config.shared.dsp().adpcm_encoder_lookahead() as i32
                    },
                    *soundtrack_config.shared.sample_rate_adjustment_curve(),
                    *soundtrack_config.shared.pitch_adjust(),
                    &global_samples_used)?);
            }

            // Write
            main_bank_swdl.save(&mut open_file_overwrite_rw(soundtrack_config.shared.outputdir().join("bgm.swd"))?, Some(*soundtrack_config.shared.flags()))?;

            for (name, song_config) in soundtrack_config.songs.iter() {
                // =========== SWDL ===========
                if let (Some(mid), Some(uses)) = (&song_config.song.mid, &song_config.song.uses) {
                    if !song_config.song.shared.decoupled() {
                        let mut swdl = SWDL::default();

                        if let Some((song_preset_map, mut samples_used, mut instrument_mappings_used)) = global_song_usage_trackers.remove(name) {
                            swdl.from_sf2_once(&soundfonts,
                                &uses,
                                get_file_last_modified_date_with_default(&mid)?,
                                name,
                                (0, 255),
                                song_config.song.shared.vcrange().clone(),
                                *song_config.song.shared.sample_rate_adjustment_curve(),
                                *song_config.song.shared.pitch_adjust(),
                                &song_preset_map,
                                &sample_mapping_information,
                                &instrument_mappings_used.get_or_insert(HashSet::new()),
                                &samples_used.get_or_insert(HashSet::new()))?;
                        } else {
                            panic!("{}Internal usage tracking structs missing for song '{}'!", "Internal Error: ".red(), name);
                        }
                        
                        swdl.set_song_builder_flags(*song_config.song.shared.flags());
                        let flags = swdl.get_song_builder_flags();
                        if flags.contains(SongBuilderFlags::STREAMED_SAMPLES) {
                            let file = &mut open_file_overwrite_rw(song_config.song.shared.outputdir().join(format!("bgm{:04}.swd", song_config.i)))?;
                            
                            if flags.contains(SongBuilderFlags::FULL_POINTER_EXTENSION) {
                                swdl.regenerate_read_markers::<Empty<u32>, u32>()?;
                                swdl.regenerate_automatic_parameters()?;
                                swdl.write_to_file::<Empty<u32>, u32, _>(file)?;
                            } else if flags.contains(SongBuilderFlags::WAVI_POINTER_EXTENSION) {
                                swdl.regenerate_read_markers::<Empty<u32>, u16>()?;
                                swdl.regenerate_automatic_parameters()?;
                                swdl.write_to_file::<Empty<u32>, u16, _>(file)?;
                            } else if flags.contains(SongBuilderFlags::PRGI_POINTER_EXTENSION) {
                                swdl.regenerate_read_markers::<Empty<u16>, u32>()?;
                                swdl.regenerate_automatic_parameters()?;
                                swdl.write_to_file::<Empty<u16>, u32, _>(file)?;
                            } else {
                                swdl.regenerate_read_markers::<Empty<u16>, u16>()?;
                                swdl.regenerate_automatic_parameters()?;
                                swdl.write_to_file::<Empty<u16>, u16, _>(file)?;
                            }
                        } else {
                            swdl.save(&mut open_file_overwrite_rw(song_config.song.shared.outputdir().join(format!("bgm{:04}.swd", song_config.i)))?, None)?;
                        }
                    }
                }
            }
        }
    } else {
        println!("{}Failed to find soundtrack.yml file!", "Error: ".red());
    }
    Ok(())
}
