use std::{fs::File, io::{Read, Write}, path::PathBuf, collections::{HashMap, HashSet, BTreeMap}, hash::Hash};

use colored::Colorize;
use dse::{dtype::{DSEError, ReadWrite, DSELinkBytes, PointerTable}, swdl::{SWDL, sf2::{copy_presets, copy_raw_sample_data, DSPOptions, SongBuilderFlags, SetSongBuilderFlags, find_in_zones}, SampleInfo, create_swdl_shell, PRGIChunk, PCMDChunk, KGRPChunk, Keygroup}, smdl::{midi::{open_midi, get_midi_tpb, get_midi_messages_flattened, TrkChunkWriter, copy_midi_messages, ProgramUsed}, create_smdl_shell, DSEEvent}};
use fileutils::{valid_file_of_type, get_file_last_modified_date_with_default};
use indexmap::IndexMap;
use serde::{Serialize, Deserialize, Deserializer};
use soundfont::{data::SampleHeader, Preset, SoundFont2, Zone};

use crate::fileutils::open_file_overwrite_rw;

mod deserialize_with;
mod fileutils;

const VERSION: Option<&str> = option_env!("CARGO_PKG_VERSION");

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
enum Song {
    Sf2AndMidi {
        mid: PathBuf,
        uses: Vec<String>
    },
    RawAudio {
        raw: PathBuf,
        loop_point: u32
    }
}

trait MergeDefaults {
    fn merge_defaults(&mut self, other: &Self);
    fn finalize(&mut self);
}

fn deserialize_resample_at<'de, D>(deserializer: D) -> Result<Option<(f64, bool)>, D::Error>
where D: Deserializer<'de> {
    let buf = String::deserialize(deserializer)?;

    if buf.trim().starts_with("times") {
        buf.trim()[5..].trim().parse().map_err(serde::de::Error::custom).map(|x| Some((x, true)))
    } else {
        buf.trim().parse().map_err(serde::de::Error::custom).map(|x| Some((x, false)))
    }
}
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct DSPConfig {
    resample_threshold: Option<f64>,
    #[serde(deserialize_with = "deserialize_resample_at")]
    resample_at: Option<(f64, bool)>,
    adpcm_encoder_lookahead: Option<u16>
}
impl Default for DSPConfig {
    fn default() -> Self {
        DSPConfig {
            resample_threshold: Some(0.0),
            resample_at: Some((1.0, true)),
            adpcm_encoder_lookahead: Some(3)
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
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Shared {
    // SWD/SMD export
    mainbank: Option<PathBuf>,
    decoupled: Option<bool>,
    sample_rate_adjustment_curve: Option<usize>,
    pitch_adjust: Option<i64>,

    // General
    outputdir: Option<PathBuf>,
    dsp: Option<DSPConfig>,
    flags: Option<SongBuilderFlags>,
}
impl Default for Shared {
    fn default() -> Self {
        Shared {
            mainbank: None,
            decoupled: Some(false),
            sample_rate_adjustment_curve: Some(1),
            pitch_adjust: Some(0),
            outputdir: Some(PathBuf::from("./out")),
            dsp: Some(DSPConfig::default()),
            flags: Some(SongBuilderFlags::empty())
        }
    }
}
impl MergeDefaults for Shared {
    fn merge_defaults(&mut self, other: &Self) {
        if self.mainbank.is_none() && other.mainbank.is_some() {
            self.mainbank = other.mainbank.clone();
        }
        if self.decoupled.is_none() && other.decoupled.is_some() {
            self.decoupled = other.decoupled.clone();
        }
        if self.sample_rate_adjustment_curve.is_none() && other.sample_rate_adjustment_curve.is_some() {
            self.sample_rate_adjustment_curve = other.sample_rate_adjustment_curve.clone();
        }
        if self.pitch_adjust.is_none() && other.pitch_adjust.is_some() {
            self.pitch_adjust = other.pitch_adjust.clone();
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
    }
    fn finalize(&mut self) {
        self.merge_defaults(&Shared::default());
        if let Some(dsp) = self.dsp.as_mut() {
            dsp.merge_defaults(&DSPConfig::default());
        }
    }
}
impl Shared {
    pub fn mainbank(&self) -> &PathBuf {
        self.mainbank.as_ref().expect("Failed to obtain option 'mainbank'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn decoupled(&self) -> &bool {
        self.decoupled.as_ref().expect("Failed to obtain option 'decoupled'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn sample_rate_adjustment_curve(&self) -> &usize {
        self.sample_rate_adjustment_curve.as_ref().expect("Failed to obtain option 'sample_rate_adjustment_curve'! It's likely that 'finalize' was not called on the options.")
    }
    pub fn pitch_adjust(&self) -> &i64 {
        self.pitch_adjust.as_ref().expect("Failed to obtain option 'pitch_adjust'! It's likely that 'finalize' was not called on the options.")
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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct SongConfig {
    i: usize,
    #[serde(flatten)]
    song: Song,
    #[serde(flatten)]
    shared: Shared
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct SoundtrackConfig {
    soundfonts: IndexMap<String, PathBuf>,
    songs: IndexMap<String, SongConfig>
}

#[derive(Debug)]
struct SampleEntry<'a> {
    i: u16,
    sample_header: &'a SampleHeader
}
impl<'a> PartialEq for SampleEntry<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.sample_header, other.sample_header)
    }
}
impl<'a> Eq for SampleEntry<'a> {  }
impl<'a> Hash for SampleEntry<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.sample_header as *const SampleHeader).hash(state);
    }
}

#[derive(Debug)]
struct InstrumentMappingEntry<'a> {
    i: usize,
    zone: &'a Zone
}
impl<'a> PartialEq for InstrumentMappingEntry<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.zone, other.zone)
    }
}
impl<'a> Eq for InstrumentMappingEntry<'a> {  }
impl<'a> Hash for InstrumentMappingEntry<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.zone as *const Zone).hash(state);
    }
}

#[derive(Debug)]
struct PresetEntry<'a> {
    i: usize,
    preset: &'a Preset
}
impl<'a> PartialEq for PresetEntry<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.preset, other.preset)
    }
}
impl<'a> Eq for PresetEntry<'a> {  }
impl<'a> Hash for PresetEntry<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.preset as *const Preset).hash(state);
    }
}

fn main() -> Result<(), DSEError> {
    if let Ok(mut config_file) = File::open("./soundtrack.yml") {
        // Read in the configuration file
        let mut config_str = String::new();
        config_file.read_to_string(&mut config_str)?;
        let soundtrack_config: SoundtrackConfig = serde_yaml::from_str(&config_str).expect(&format!("{}Configuration file is not valid!", "Error: ".red()));
    
        // Read in all the soundfont files
        let mut soundfonts: HashMap<String, SoundFont2> = HashMap::new();
        for (name, path) in soundtrack_config.soundfonts.iter() {
            println!("[*] Opening soundfont {:?}", path);
            soundfonts.insert(name.clone(), SoundFont2::load(&mut File::open(path)?).map_err(|x| DSEError::SoundFontParseError(format!("{:?}", x)))?);
        }

        // =========== SMDL ===========

        // Keep track of how the presets are mapped
        let mut preset_mapping_information: HashMap<String, HashMap<(u8, u8), u8>> = HashMap::new();

        // Keep track of which presets and samples are actually used by the MIDI
        let mut samples_used: HashMap<String, HashSet<SampleEntry>> = HashMap::new();
        let mut samples_used_per_song: HashMap<String, HashSet<SampleEntry>> = HashMap::new();
        let mut instrument_mappings_used: HashMap<String, HashSet<InstrumentMappingEntry>> = HashMap::new();
        let mut presets_used: HashMap<String, HashSet<PresetEntry>> = HashMap::new();
        let mut presets_used_per_song: HashMap<String, HashSet<PresetEntry>> = HashMap::new();

        // Read in all the MIDI files
        for (name, song_config) in soundtrack_config.songs.iter() {
            println!("[*] Reading MIDI file {:?}", song_config.mid);
            let smf_source = std::fs::read(&song_config.mid)?;
            let smf = open_midi(&smf_source)?;
            let tpb = get_midi_tpb(&smf)?;

            fn find_preset_in_soundfont(soundfont: &SoundFont2, bank: u16, program: u16) -> Option<usize> {
                for (i, preset) in soundfont.presets.iter().enumerate() {
                    if preset.header.bank == bank && preset.header.preset == program {
                        return Some(i);
                    }
                }
                return None;
            }
            fn find_preset_in_soundfonts<'a>(soundfonts: &'a [&SoundFont2], bank: u16, program: u16) -> Option<(usize, usize)> {
                for (soundfont_i, soundfont) in soundfonts.iter().enumerate() {
                    if let Some(preset_i) = find_preset_in_soundfont(soundfont, bank, program) {
                        return Some((soundfont_i, preset_i));
                    }
                }
                return None;
            }

            let mut smdl = create_smdl_shell(get_file_last_modified_date_with_default(&song_config.mid)?, format!("{}.SMD", name))?;
            smdl.set_link_bytes((0, 255));
            smdl.song.tpqn = tpb;

            let midi_messages = get_midi_messages_flattened(&smf)?;

            // Copy midi messages
            let mut used_presets: IndexMap<u8, Vec<(usize, (u8, u8))>> = IndexMap::new();
            let mut map_program = |trkid, bank, program, same_tick, trk_chunk_writer: &mut TrkChunkWriter| {
                if same_tick {
                    // Discard last. If same_tick is true, then a previous preset change has already been recorded, so this is guaranteed to remove the correct entry.
                    used_presets.entry(trkid).or_insert(Vec::new()).pop();
                }
                used_presets.entry(trkid).or_insert(Vec::new()).push((trk_chunk_writer.next_event_index(), (bank, program)));
                // Insert the event for now, fixing it later to be the correct value
                Some(0)
            };
            // Vec of TrkChunkWriter's
            let mut trks: [TrkChunkWriter; 17] = std::array::from_fn(|i| {
                let mut trk = TrkChunkWriter::create(i as u8, i as u8, smdl.get_link_bytes()).unwrap();
                // Most soundfont players default to preset 000:000 if no MIDI Bank Select and Program Change messages are found. This matches that behavior.
                if i != 0 { // Avoid writing the default patch select messages onto the meta track
                    let _ = trk.bank_select(0, true, &mut map_program); // The results can be ignored since the only failure condition is if the DSE opcode "SetProgram" could not be found, which would be very bad if that happened and this wouldn't be able to recover anyways.
                    let _ = trk.program_change(0, true, &mut map_program);
                }
                trk
            });
            let _ = copy_midi_messages(midi_messages, &mut trks, &mut map_program)?;
            let mut song_preset_map: HashMap<(u8, u8), u8> = HashMap::new();
            let mut current_id = 0_u8;
            for (trkid, used_presets) in used_presets.into_iter() {
                for (event_index, (bank, program)) in used_presets {
                    println!("trk{:02} {} bank{} prgm{}", trkid, event_index, bank, program);
                    let program_id;
                    if let Some(&existing_program_id) = song_preset_map.get(&(bank, program)) {
                        program_id = existing_program_id;
                    } else {
                        // Assign new
                        let assigned_id = current_id;
                        current_id += 1;
                        song_preset_map.insert((bank, program), assigned_id);
                        program_id = assigned_id;
                    }
                    if let Some(evt) = trks[trkid as usize].get_event_mut(event_index) {
                        if let DSEEvent::Other(evt) = &mut evt.1 {
                            (&mut evt.parameters[..]).write_all(&[program_id])?;
                        }
                    } else {
                        panic!("{}TrkChunkWriter's internal event list must never have items removed from it! However, a previously valid index is now missing!!", "Internal Error: ".red());
                    }
                }
            }

            // Fill the tracks into the smdl
            let track_soundfonts = song_config.uses.iter().map(|soundfont_name| soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))).collect::<Result<Vec<&SoundFont2>, _>>()?;
            smdl.trks.objects = Vec::with_capacity(trks.len());
            for x in trks {
                for ProgramUsed { bank, program, notes, is_default } in x.programs_used() {
                    let find_preset = find_preset_in_soundfonts(&track_soundfonts, *bank as u16, *program as u16);
                    if find_preset.is_none() && *is_default {
                        println!("{}None of the following soundfonts {:?} used by a track contain a default 000:000 piano preset! Any MIDI tracks lacking MIDI Bank Select and Program Change messages will cause the tool to fail!", "Warning: ".yellow(), song_config.uses);
                        continue;
                    }
                    let (soundfont_i, preset_i) = find_preset.ok_or(DSEError::Invalid(format!("Preset {:03}:{:03} not found in any of the specified soundfonts for song '{}'!", bank, program, name)))?;
                    let sf2 = soundfonts.get(&song_config.uses[soundfont_i]).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", &song_config.uses[soundfont_i])))?;
                    presets_used.entry(song_config.uses[soundfont_i].clone()).or_insert(HashSet::new())
                        .insert(PresetEntry { i: preset_i, preset: &sf2.presets[preset_i] });
                    presets_used_per_song.entry(name.clone()).or_insert(HashSet::new())
                        .insert(PresetEntry { i: preset_i, preset: &sf2.presets[preset_i] });

                    let mut dummy_prgi = PointerTable::new(0, 0);
                    let mut preset_zones_used_for_soundfont_indices = Vec::new();
                    copy_presets(sf2, &mut (0..sf2.sample_headers.len()).into_iter().map(|i| {
                        let mut dummy_smpl = SampleInfo::default();
                        dummy_smpl.smplrate = 44100;
                        (i as u16, dummy_smpl)
                    }).collect::<BTreeMap<u16, SampleInfo>>(), &mut dummy_prgi, |x| Some(x), 1, 0, |preset, global_preset_zone, preset_zone_i, preset_zone, instrument_i, instrument| {
                        // When this is called, the instrument is guaranteed to not be a global instrument
                        let mut preset_zones_to_search = vec![preset_zone];
                        if let Some(global_preset_zone) = global_preset_zone {
                            preset_zones_to_search.push(global_preset_zone);
                        }
                        // By default, keep the instrument
                        let mut keep = preset.header.bank == *bank as u16 && preset.header.preset == *program as u16;
                        let key_range;
                        let vel_range;
                        // Check the instrument's key range, if it is specified
                        if let Some(gen) = find_in_zones(&preset_zones_to_search, soundfont::data::GeneratorType::KeyRange) {
                            let key_range_value = gen.amount.as_range().unwrap();
                            let lowkey = key_range_value.low as i8;
                            let hikey = key_range_value.high as i8;
                            key_range = Some(lowkey as u8..=hikey as u8);
                        } else {
                            key_range = None;
                        }
                        // Check the instrument's velocity range, if it is specified
                        if let Some(gen) = find_in_zones(&preset_zones_to_search, soundfont::data::GeneratorType::VelRange) {
                            let vel_range_value = gen.amount.as_range().unwrap();
                            let lovel = vel_range_value.low as i8;
                            let hivel = vel_range_value.high as i8;
                            vel_range = Some(lovel as u8..=hivel as u8);
                        } else {
                            vel_range = None;
                        }
                        // Check for all possibilities of the two ranges existing
                        if let (Some(key_range), Some(vel_range)) = (&key_range, &vel_range) {
                            keep = keep && notes.iter().any(|(key, vels)| key_range.contains(key) && vels.iter().any(|vel| vel_range.contains(vel)));
                        } else if let Some(key_range) = &key_range {
                            keep = keep && notes.iter().any(|(key, _)| key_range.contains(key));
                        } else if let Some(vel_range) = &vel_range {
                            keep = keep && notes.iter().any(|(_, vels)| vels.iter().any(|vel| vel_range.contains(vel)));
                        }
                        // Make a record of if this instrument is used or not (only the index can be saved, and so a second step is necessary to actually turn these indices into references, which is done outside of this closure)
                        if keep {
                            preset_zones_used_for_soundfont_indices.push(preset_zone_i);
                        }
                        keep
                    }, |_, preset, _| {
                        if preset.header.bank == *bank as u16 && preset.header.preset == *program as u16 {
                            Some(0)
                        } else {
                            None
                        }
                    });
                    // Turn the preset zone (individual instrument mappings) indices for instruments used into actual references to the preset zones
                    for preset_zone_i in preset_zones_used_for_soundfont_indices {
                        if let Some(preset) = sf2.presets.iter().find(|preset| preset.header.bank == *bank as u16 && preset.header.preset == *program as u16) {
                            instrument_mappings_used.entry(song_config.uses[soundfont_i].clone()).or_insert(HashSet::new())
                                .insert(InstrumentMappingEntry { i: preset_zone_i, zone: &preset.zones[preset_zone_i as usize] });
                        } else {
                            panic!("{}Failed to find a previously-marked preset!", "Internal Error: ".red());
                        }
                    }
                    //TODO: An sf2 exported from VGMTrans had an extra empty preset after all the normal ones visible in Polyphone with a bank/preset number of 000:000, which broke the assertion that each id should correspond to one preset. The likely explanation is that empty presets are meant to be ignored, and so we do that here.
                    dummy_prgi.objects.retain(|x| {
                        x.splits_table.len() > 0
                    });
                    assert!(dummy_prgi.objects.len() <= 1); //TODO: Low priority, but replace this with an actual error. This should never happen.
                    for program in dummy_prgi.objects {
                        for split in program.splits_table.objects {
                            let key_range = split.lowkey as u8..=split.hikey as u8;
                            let vel_range = split.lovel as u8..=split.hivel as u8;
                            if notes.iter().any(|(key, vels)| key_range.contains(key) && vels.iter().any(|vel| vel_range.contains(vel))) {
                                samples_used.entry(song_config.uses[soundfont_i].clone()).or_insert(HashSet::new())
                                    .insert(SampleEntry { i: split.SmplID, sample_header: &sf2.sample_headers[split.SmplID as usize] });
                                samples_used_per_song.entry(name.clone()).or_insert(HashSet::new())
                                    .insert(SampleEntry { i: split.SmplID, sample_header: &sf2.sample_headers[split.SmplID as usize] });
                            }
                        }
                    }
                }
                smdl.trks.objects.push(x.close_track());
            }

            // Regenerate read markers for the SMDL
            smdl.regenerate_read_markers()?;

            // Write to file
            smdl.write_to_file(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join(format!("bgm{:04}.smd", song_config.i)))?)?;
        
            preset_mapping_information.insert(name.clone(), song_preset_map);
        }

        // =========== MAIN BANK SWDL ===========

        // Read in the main bank swd file
        let mut main_bank_swdl;
        if valid_file_of_type(&soundtrack_config.mainbank, "swd") {
            let flags = SongBuilderFlags::parse_from_swdl_file(&mut File::open(soundtrack_config.mainbank.clone())?)?;

            main_bank_swdl = SWDL::default();
            println!("[*] Opening mainbank {:?}", soundtrack_config.mainbank);
            if flags.contains(SongBuilderFlags::FULL_POINTER_EXTENSION) {
                main_bank_swdl.read_from_file::<u32, u32, _>(&mut File::open(&soundtrack_config.mainbank)?)?;
            } else if flags.contains(SongBuilderFlags::WAVI_POINTER_EXTENSION) {
                main_bank_swdl.read_from_file::<u32, u16, _>(&mut File::open(&soundtrack_config.mainbank)?)?;
            } else if flags.contains(SongBuilderFlags::PRGI_POINTER_EXTENSION) {
                main_bank_swdl.read_from_file::<u16, u32, _>(&mut File::open(&soundtrack_config.mainbank)?)?;
            } else {
                main_bank_swdl.read_from_file::<u16, u16, _>(&mut File::open(&soundtrack_config.mainbank)?)?;
            }
        } else if valid_file_of_type(&soundtrack_config.mainbank, "xml") {
            let st = std::fs::read_to_string(&soundtrack_config.mainbank)?;
            main_bank_swdl = quick_xml::de::from_str::<SWDL>(&st)?;

            let flags = SongBuilderFlags::parse_from_swdl(&main_bank_swdl);

            if flags.contains(SongBuilderFlags::FULL_POINTER_EXTENSION) {
                main_bank_swdl.regenerate_read_markers::<u32, u32>()?;
            } else if flags.contains(SongBuilderFlags::WAVI_POINTER_EXTENSION) {
                main_bank_swdl.regenerate_read_markers::<u32, u16>()?;
            } else if flags.contains(SongBuilderFlags::PRGI_POINTER_EXTENSION) {
                main_bank_swdl.regenerate_read_markers::<u16, u32>()?;
            } else {
                main_bank_swdl.regenerate_read_markers::<u16, u16>()?;
            }

            main_bank_swdl.regenerate_automatic_parameters()?;
        } else {
            return Err(DSEError::Invalid("Provided Main Bank SWD file is not an SWD file!".to_string()));
        }

        // Start patching in the SF2 files one by one into the main bank, keeping a record of how samples are mapped for each one
        let mut sample_mapping_information: HashMap<String, (HashMap<u16, u16>, BTreeMap<u16, SampleInfo>)> = HashMap::new();
        for (soundfont_name, sf2) in soundfonts.iter() {
            // If this check fails, that just means that this soundfont isn't used at all. Skip.
            if let Some(samples_used) = samples_used.get(soundfont_name) {
                println!("[*] Opening soundfont {:?} for patching", &soundtrack_config.soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?);
                let (sample_rate, sample_rate_relative) = soundtrack_config.dsp.resample_at;
                sample_mapping_information.insert(soundfont_name.clone(), copy_raw_sample_data(
                    &File::open(&soundtrack_config.soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?)?,
                    sf2,
                    &mut main_bank_swdl,
                    DSPOptions {
                        resample_threshold: soundtrack_config.dsp.resample_threshold.round() as u32,
                        sample_rate,
                        sample_rate_relative,
                        adpcm_encoder_lookahead: soundtrack_config.dsp.adpcm_encoder_lookahead as i32
                    },
                    soundtrack_config.sample_rate_adjustment_curve,
                    soundtrack_config.pitch_adjust,
                    |_, sample_header| samples_used.contains(&SampleEntry { i: 0, sample_header }))?);
            } else {
                println!("{}Soundfont '{}' is never used!", "Warning: ".yellow(), soundfont_name);
            }
        }

        // Write to file
        if !soundtrack_config.decoupled {
            main_bank_swdl.set_song_builder_flags(soundtrack_config.flags);
            if soundtrack_config.flags.contains(SongBuilderFlags::FULL_POINTER_EXTENSION) {
                main_bank_swdl.regenerate_read_markers::<u32, u32>()?;
                main_bank_swdl.regenerate_automatic_parameters()?;
                main_bank_swdl.write_to_file::<u32, u32, _>(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join("bgm.swd"))?)?;
            } else if soundtrack_config.flags.contains(SongBuilderFlags::WAVI_POINTER_EXTENSION) {
                main_bank_swdl.regenerate_read_markers::<u32, u16>()?;
                main_bank_swdl.regenerate_automatic_parameters()?;
                main_bank_swdl.write_to_file::<u32, u16, _>(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join("bgm.swd"))?)?;
            } else if soundtrack_config.flags.contains(SongBuilderFlags::PRGI_POINTER_EXTENSION) {
                main_bank_swdl.regenerate_read_markers::<u16, u32>()?;
                main_bank_swdl.regenerate_automatic_parameters()?;
                main_bank_swdl.write_to_file::<u16, u32, _>(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join("bgm.swd"))?)?;
            } else {
                main_bank_swdl.regenerate_read_markers::<u16, u16>()?;
                main_bank_swdl.regenerate_automatic_parameters()?;
                main_bank_swdl.write_to_file::<u16, u16, _>(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join("bgm.swd"))?)?;
            }
        }
    
        // =========== SWDL ===========

        for (name, song_config) in soundtrack_config.songs.iter() {
            // Create a blank track SWDL file
            let mut swdl = create_swdl_shell(get_file_last_modified_date_with_default(&song_config.mid)?, format!("{}.SWD", name))?;
            swdl.set_link_bytes((0, 255));

            // Get the song's preset mapping information
            let song_preset_map = preset_mapping_information.get(name).ok_or(DSEError::WrapperString(format!("{}Song missing from `preset_mapping_information`!", "Internal Error: ".red())))?;

            // Get the soundfonts used by the track
            let track_soundfonts = song_config.uses.iter().map(|soundfont_name| soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))).collect::<Result<Vec<&SoundFont2>, _>>()?;

            // Copy over the necessary presets from the used soundfonts
            let mut prgi = PRGIChunk::new(0);
            let mut sample_infos_merged = BTreeMap::new();
            for (soundfont_name, &sf2) in song_config.uses.iter().zip(track_soundfonts.iter()) {
                if let Some((sample_mappings, sample_infos)) = sample_mapping_information.get(soundfont_name) {
                    if let Some(instrument_mappings_used_for_soundfont) = instrument_mappings_used.get(soundfont_name) {
                        let mut sample_infos = sample_infos.clone();
                        copy_presets(
                            sf2,
                            &mut sample_infos,
                            &mut prgi.data,
                            |i| sample_mappings.get(&i).copied(),
                            soundtrack_config.sample_rate_adjustment_curve,
                            soundtrack_config.pitch_adjust,
                            |_, _, _, preset_zone, _, _| instrument_mappings_used_for_soundfont.get(&InstrumentMappingEntry { i: 0, zone: preset_zone }).is_some(),
                            |_, preset, program_info| {
                                //TODO: An sf2 exported from VGMTrans had an extra empty preset after all the normal ones visible in Polyphone with a bank/preset number of 000:000, which broke the assertion that each id should correspond to one preset. The likely explanation is that empty presets are meant to be ignored, and so we do that here.
                                if program_info.splits_table.len() > 0 {
                                    song_preset_map.get(&(preset.header.bank as u8, preset.header.preset as u8)).map(|x| *x as u16)   
                                } else {
                                    None
                                }
                            });
                        let sample_infos_trimmed: BTreeMap<u16, SampleInfo> = samples_used_per_song.get(name).ok_or(DSEError::Invalid(format!("Song with name '{}' not found!", &name)))?.iter().filter_map(|x| {
                            if let Some(mapping) = sample_mappings.get(&x.i) {
                                Some((x.i, sample_infos.get(mapping).ok_or(DSEError::_SampleInPresetMissing(*mapping)).unwrap().clone()))
                            } else {
                                // The ones that are filtered out are not in this specific soundfont
                                None
                            }
                        }).collect::<BTreeMap<u16, SampleInfo>>();
                        sample_infos_merged.extend(sample_infos_trimmed);
                    }
                } else {
                    println!("{}Soundfont '{}' is never used! Writing will be skipped.", "Warning: ".yellow(), soundfont_name);
                }
            }
            swdl.prgi = Some(prgi);

            // Add the sample info objects last
            swdl.wavi.data.objects = sample_infos_merged.into_values().collect();
            // Fix the smplpos
            if soundtrack_config.decoupled {
                let mut pcmd = PCMDChunk::default();
                if let Some(main_pcmd) = &mut main_bank_swdl.pcmd {
                    let main_wavi = &main_bank_swdl.wavi;
                    for obj in &mut swdl.wavi.data.objects {
                        // This part is awful, there may be a way to optimize searching the matching sample info in the main bank, but I don't know how
                        if let Some(obj_main) = &main_wavi.data.objects.iter().filter(|item| item.id == obj.id).next() {
                            obj.smplpos = pcmd.data.len() as u32;
                            pcmd.data.extend(&main_pcmd.data[(obj_main.smplpos as usize)..((obj_main.smplpos + (obj_main.loopbeg + obj_main.looplen) * 4) as usize)]);
                        }
                    }
                }
                swdl.pcmd = Some(pcmd);
            } else {
                let mut pos_in_memory = 0;
                for obj in &mut swdl.wavi.data.objects {
                    obj.smplpos = pos_in_memory;
                    pos_in_memory += (obj.loopbeg + obj.looplen) * 4;
                }
            }

            // Keygroups
            let mut kgrp = KGRPChunk::default();
            kgrp.data.objects = vec![
                Keygroup { id: 0, poly: -1, priority: 8, vclow: 0, vchigh: -1, unk50: 0, unk51: 0 },
                Keygroup { id: 1, poly: 2, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 2, poly: 1, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 3, poly: 1, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 4, poly: 1, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 5, poly: 1, priority: 1, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 6, poly: 2, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 7, poly: 1, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 8, poly: 2, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 9, poly: -1, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 10, poly: -1, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
                Keygroup { id: 11, poly: -1, priority: 8, vclow: 0, vchigh: 15, unk50: 0, unk51: 0 },
            ]; // Just a quick template keygroup list. By default only the first kgrp is used!
            swdl.kgrp = Some(kgrp);

            // Write to file
            swdl.set_song_builder_flags(soundtrack_config.flags);
            if soundtrack_config.flags.contains(SongBuilderFlags::FULL_POINTER_EXTENSION) {
                swdl.regenerate_read_markers::<u32, u32>()?;
                swdl.regenerate_automatic_parameters()?;
                swdl.write_to_file::<u32, u32, _>(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join(format!("bgm{:04}.swd", song_config.i)))?)?;
            } else if soundtrack_config.flags.contains(SongBuilderFlags::WAVI_POINTER_EXTENSION) {
                swdl.regenerate_read_markers::<u32, u16>()?;
                swdl.regenerate_automatic_parameters()?;
                swdl.write_to_file::<u32, u16, _>(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join(format!("bgm{:04}.swd", song_config.i)))?)?;
            } else if soundtrack_config.flags.contains(SongBuilderFlags::PRGI_POINTER_EXTENSION) {
                swdl.regenerate_read_markers::<u16, u32>()?;
                swdl.regenerate_automatic_parameters()?;
                swdl.write_to_file::<u16, u32, _>(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join(format!("bgm{:04}.swd", song_config.i)))?)?;
            } else {
                swdl.regenerate_read_markers::<u16, u16>()?;
                swdl.regenerate_automatic_parameters()?;
                swdl.write_to_file::<u16, u16, _>(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join(format!("bgm{:04}.swd", song_config.i)))?)?;
            }
        }
    } else {
        println!("{}Failed to find soundtrack.yml file!", "Error: ".red());
    }
    Ok(())
}
