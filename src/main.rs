use std::{fs::File, io::{Read, Cursor, Seek}, path::PathBuf, collections::{HashMap, HashSet, BTreeMap}, ops::RangeInclusive, str::FromStr};

use bevy_reflect::Reflect;
use colored::Colorize;
use dse::{dsp::{process_mono, init_deltas, adpcm_encode_round_to_valid_block_size, block_alignment::BlockAlignment, resample_len_preview, get_sample_rate_by_out_samples, adpcm_block_size_preview}, dtype::{DSEError, SongBuilderFlags, AutoReadWrite, ReadWrite}, swdl::{SWDL, sf2::DSPOptions, SampleInfo}, smdl::{midi::open_midi, SMDL}, opinionated_translators::sf2midi::{FromMIDIOnce, TrimmedSampleDataCopy, FromSF2Once}};
use fileutils::get_file_last_modified_date_with_default;
use indexmap::IndexMap;
use riff::{ChunkId, ChunkContents};
use serde::{Serialize, Deserialize, Deserializer, de};
use serde_yaml::Value;
use soundfont::SoundFont2;
use symphonia::core::{io::MediaSourceStream, probe::Hint, formats::FormatOptions, meta::MetadataOptions, codecs::DecoderOptions, audio::SampleBuffer};
use void::Void;

use crate::fileutils::{open_file_overwrite_rw, get_now};

mod deserialize_with;
mod fileutils;

use deserialize_with::string_or_struct;

const VERSION: Option<&str> = option_env!("CARGO_PKG_VERSION");
static EMPTY_MIDI: &'static [u8] = include_bytes!("./empty.mid");
static EMPTY_SF2: &'static [u8] = include_bytes!("./empty.sf2");

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum Midi {
    #[serde(skip)]
    InMemoryEmpty(),
    External(PathBuf)
}
#[derive(Debug, Serialize, Deserialize)]
struct Song {
    mid: Option<Midi>,
    uses: Option<Vec<String>>,

    raw: Option<PathBuf>,
    loop_point: Option<usize>,

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
    Wav4BitAdpcmNDSPlanarStereoLooped,
}
impl RawAudioExportFormats {
    pub fn to_file_string(&self) -> String {
        match self {
            RawAudioExportFormats::Wav4BitAdpcmNDSPlanarStereoLooped => "adpcm".to_string(),
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

    // General
    outputdir: Option<PathBuf>,
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

            formats: Some(HashSet::from_iter(std::iter::once(RawAudioExportFormats::Wav4BitAdpcmNDSPlanarStereoLooped))),

            outputdir: Some(PathBuf::from("./out")),
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
        if self.outputdir.is_none() && other.outputdir.is_some() {
            self.outputdir = other.outputdir.clone();
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
    pub fn outputdir(&self) -> &PathBuf {
        self.outputdir.as_ref().expect("Failed to obtain option 'outputdir'! It's likely that 'finalize' was not called on the options.")
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

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum Soundfont {
    #[serde(skip)]
    InMemoryEmpty(),
    External(PathBuf)
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct SoundfontConfig {
    soundfont: Soundfont,
}
impl FromStr for SoundfontConfig {
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(SoundfontConfig {
            soundfont: Soundfont::External(PathBuf::from(s)),
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
    #[serde(deserialize_with = "soundfonts_map")]
    soundfonts: IndexMap<String, SoundfontConfig>,
    songs: IndexMap<String, SongConfig>
}

trait ReadSeek: Read + Seek {  }
impl<R: Read + Seek> ReadSeek for R {  }

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

        // Register the built-in empty Soundfont under the name "<empty>" if it has not been overridden by the user
        if !soundtrack_config.soundfonts.contains_key("<empty>") {
            soundtrack_config.soundfonts.insert("<empty>".to_string(), SoundfontConfig { soundfont: Soundfont::InMemoryEmpty() });
        }

        // Read in all the soundfont files
        let mut soundfonts: HashMap<String, SoundFont2> = HashMap::new();
        for (name, soundfont_config) in soundtrack_config.soundfonts.iter() {
            println!("[*] Opening soundfont {:?}", soundfont_config);
            soundfonts.insert(name.clone(), match &soundfont_config.soundfont {
                Soundfont::InMemoryEmpty() => SoundFont2::load(&mut Cursor::new(EMPTY_SF2)).map_err(|x| DSEError::SoundFontParseError(format!("{:?}", x)))?,
                Soundfont::External(path) => SoundFont2::load(&mut File::open(path)?).map_err(|x| DSEError::SoundFontParseError(format!("{:?}", x)))?,
            });
        }

        // =========== SMDL ===========

        let mut global_samples_used = None;
        let mut global_song_usage_trackers = None;

        for (name, song_config) in soundtrack_config.songs.iter_mut() {
            let mut process_sf2_and_midi = |mid: &Midi, uses: &Vec<String>, song: &Song, soundfont_configs: &IndexMap<String, SoundfontConfig>, soundfonts: &HashMap<String, SoundFont2>| -> Result<(), DSEError> {
                println!("[*] Reading MIDI file {:?}", &mid);
                let smf_source = match mid {
                    Midi::InMemoryEmpty() => Vec::from(EMPTY_MIDI),
                    Midi::External(mid) => std::fs::read(&mid)?,
                };
                let smf = open_midi(&smf_source)?;

                let mut smdl = SMDL::default();
                let (song_preset_map, mut samples_used, mut instrument_mappings_used, _) = smdl.from_midi_once(&smf,
                    match mid {
                        Midi::InMemoryEmpty() => get_now(),
                        Midi::External(mid) => get_file_last_modified_date_with_default(&mid)?,
                    },
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
                            &mut match &soundfont_config.soundfont {
                                Soundfont::InMemoryEmpty() => Box::new(Cursor::new(EMPTY_SF2)) as Box<dyn ReadSeek>,
                                Soundfont::External(path) => Box::new(File::open(path)?) as Box<dyn ReadSeek>,
                            },
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
                        match mid {
                            Midi::InMemoryEmpty() => get_now(),
                            Midi::External(mid) => get_file_last_modified_date_with_default(&mid)?,
                        },
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
            let process_raw_audio = |raw: PathBuf, loop_point: Option<usize>, song: &mut Song| -> Result<(), DSEError> {
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

                for format in song.shared.formats().iter() {
                    match format {
                        RawAudioExportFormats::Wav4BitAdpcmNDSPlanarStereoLooped => {
                            // Figure out the block size
                            let block_size = adpcm_encode_round_to_valid_block_size(if let Some(adpcm_block_size) = song.shared.dsp().adpcm_block_size() {
                                *adpcm_block_size
                            } else {
                                left_samples.len()
                            });

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

                            // Process
                            let left_samples_processed;
                            let right_samples_processed;
                            pub struct ToBlocks {
                                multiple: usize
                            }
                            impl ToBlocks {
                                pub fn new(multiple: usize) -> ToBlocks {
                                    ToBlocks { multiple }
                                }
                            }
                            impl BlockAlignment for ToBlocks {
                                fn round_up(&self, value: usize) -> usize {
                                    if self.multiple == 0 {
                                        return value;
                                    }
                                    let remainder = value % self.multiple;
                                    if remainder == 0 {
                                        return value;
                                    }
                                    return value + self.multiple - remainder;
                                }
                                fn zero_pad_front(&self, _: usize) -> usize {
                                    unimplemented!()
                                }
                                fn generate_aligned_options(&self, _: usize) -> Vec<usize> {
                                    unimplemented!()
                                }
                            }
                            
                            let mut resampled_len_preview = resample_len_preview(track_sample_rate as f64, new_sample_rate, left_samples.len());
                            if let Some(_) = loop_point {
                                let target_n_samples = ToBlocks::new(block_size).round_up(resampled_len_preview);
                                new_sample_rate = get_sample_rate_by_out_samples(track_sample_rate as f64, left_samples.len(), target_n_samples);
                                resampled_len_preview = target_n_samples;
                            }
                            (left_samples_processed, _) = process_mono(&left_samples,
                                track_sample_rate as f64, new_sample_rate,
                                *song.shared.dsp().adpcm_encoder_lookahead() as i32, init_deltas::averaging,
                                Some(block_size), &[]);
                            (right_samples_processed, _) = process_mono(&right_samples,
                                track_sample_rate as f64, new_sample_rate,
                                *song.shared.dsp().adpcm_encoder_lookahead() as i32, init_deltas::averaging,
                                Some(block_size), &[]);
                            assert_eq!(left_samples_processed.len(), right_samples_processed.len());
                            resampled_len_preview = adpcm_block_size_preview(block_size) * (resampled_len_preview / block_size);
                            assert_eq!(left_samples_processed.len(), resampled_len_preview);

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
                                cbSize: u16
                            }
                            impl AutoReadWrite for WaveAdpcmFmtChunk {  }
                            // let extraData = [0; 32];
                            let fmt_data = {
                                let mut data: Vec<u8> = Vec::new();
                                let mut cursor = Cursor::new(&mut data);
                                WaveAdpcmFmtChunk {
                                    wFormatTag: 0x11,
                                    nChannels: 1,
                                    nSamplesPerSec: new_sample_rate as u32,
                                    nAvgBytesPerSec: (((new_sample_rate / 2.0).ceil() as u32 - 1) | 255) + 1,
                                    nBlockAlign: adpcm_block_size_preview(block_size) as u16,
                                    wBitsPerSample: 4,
                                    cbSize: 0
                                }.write_to_file(&mut cursor)?;
                                data
                            };

                            #[derive(Debug, Clone, Default, Reflect)]
                            struct WaveSmplChunk {
                                manufacturer: u32,
                                product: u32,
                                sample_period: u32,
                                midi_unity_note: u32,
                                midi_pitch_fraction: u32,
                                smpte_format: u32,
                                smpte_offset: u32,
                                num_loops: u32,
                                sample_data: u32,
                                loop_1_id: u32,
                                loop_1_type: u32,
                                loop_1_start: u32,
                                loop_1_end: u32,
                                loop_1_fraction: u32,
                                loop_1_num_times_to_play_the_loop: u32
                            }
                            impl AutoReadWrite for WaveSmplChunk {  }
                            let smpl_data = if let Some(loop_point) = loop_point {
                                let mut data: Vec<u8> = Vec::new();
                                let mut cursor = Cursor::new(&mut data);
                                WaveSmplChunk {
                                    manufacturer: 0,
                                    product: 0,
                                    sample_period: (1_000_000_000.0 / new_sample_rate).round() as u32,
                                    midi_unity_note: 60,
                                    midi_pitch_fraction: 0,
                                    smpte_format: 0,
                                    smpte_offset: 0,
                                    num_loops: 1,
                                    sample_data: 0,
                                    loop_1_id: 0,
                                    loop_1_type: 0,
                                    loop_1_start: loop_point as u32,
                                    loop_1_end: loop_point as u32,
                                    loop_1_fraction: 0,
                                    loop_1_num_times_to_play_the_loop: 0,
                                }.write_to_file(&mut cursor)?;
                                Some(data)
                            } else {
                                None
                            };

                            ChunkContents::Children(
                                riff::RIFF_ID,
                                wave_id,
                                if let Some(smpl_data) = smpl_data.as_ref() {
                                    vec![ChunkContents::Data(fmt_id, fmt_data.clone()),
                                        ChunkContents::Data(smpl_id, smpl_data.clone()),
                                        ChunkContents::Data(data_id, left_samples_processed)]
                                } else {
                                    vec![ChunkContents::Data(fmt_id, fmt_data.clone()),
                                        ChunkContents::Data(data_id, left_samples_processed)]
                                }
                            ).write(&mut left_file)?;
                            ChunkContents::Children(
                                riff::RIFF_ID,
                                wave_id,
                                if let Some(smpl_data) = smpl_data.as_ref() {
                                    vec![ChunkContents::Data(fmt_id, fmt_data.clone()),
                                        ChunkContents::Data(smpl_id, smpl_data.clone()),
                                        ChunkContents::Data(data_id, right_samples_processed)]
                                } else {
                                    vec![ChunkContents::Data(fmt_id, fmt_data.clone()),
                                        ChunkContents::Data(data_id, right_samples_processed)]
                                }
                            ).write(&mut right_file)?;
                        }
                    }
                }

                // Set the song to use the in memory empty midi template if a midi file is not set so that a paired swd/smd set can be built for this streamed song.
                if song.mid.is_none() {
                    song.mid = Some(Midi::InMemoryEmpty());
                }

                // Set the song to use soundfont "<empty>" so that a paired swd/smd set can be built for this song if requested later.
                let uses = song.uses.get_or_insert(Vec::new());
                if !uses.contains(&"<empty>".to_string()) {
                    uses.push("<empty>".to_string());
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
                let soundfont = &soundtrack_config.soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?.soundfont;
                sample_mapping_information.insert(soundfont_name.clone(), main_bank_swdl.trimmed_raw_sample_copy(soundfont_name,
                    &mut match soundfont {
                        Soundfont::InMemoryEmpty() => Box::new(Cursor::new(EMPTY_SF2)) as Box<dyn ReadSeek>,
                        Soundfont::External(path) => Box::new(File::open(path)?) as Box<dyn ReadSeek>,
                    },
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
                                match mid {
                                    Midi::InMemoryEmpty() => get_now(),
                                    Midi::External(mid) => get_file_last_modified_date_with_default(&mid)?,
                                },
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
                        
                        swdl.save(&mut open_file_overwrite_rw(song_config.song.shared.outputdir().join(format!("bgm{:04}.swd", song_config.i)))?, Some(*song_config.song.shared.flags()))?;
                    }
                }
            }
        }
    } else {
        println!("{}Failed to find soundtrack.yml file!", "Error: ".red());
    }
    Ok(())
}
