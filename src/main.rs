use std::{fs::File, io::Read, path::PathBuf, collections::{HashMap, HashSet, BTreeMap}, ops::RangeInclusive};

use colored::Colorize;
use dse::{dtype::{DSEError, ReadWrite}, swdl::{SWDL, SongBuilderFlags, sf2::DSPOptions, SampleInfo}, smdl::{midi::open_midi, SMDL}, opinionated_translators::sf2midi::{FromMIDIOnce, TrimmedSampleDataCopy, FromSF2Once}};
use fileutils::get_file_last_modified_date_with_default;
use indexmap::IndexMap;
use serde::{Serialize, Deserialize, Deserializer};
use soundfont::SoundFont2;

use crate::fileutils::open_file_overwrite_rw;

use thiserror::Error;

mod deserialize_with;
mod fileutils;

const VERSION: Option<&str> = option_env!("CARGO_PKG_VERSION");

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct SongConfig {
    i: usize,
    mid: PathBuf,
    uses: Vec<String>
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct DSPConfig {
    resample_threshold: f64,
    #[serde(deserialize_with = "deserialize_resample_at")]
    resample_at: (f64, bool),
    adpcm_encoder_lookahead: u16
}
fn deserialize_resample_at<'de, D>(deserializer: D) -> Result<(f64, bool), D::Error>
where D: Deserializer<'de> {
    let buf = String::deserialize(deserializer)?;

    if buf.trim().starts_with("times") {
        buf.trim()[5..].trim().parse().map_err(serde::de::Error::custom).map(|x| (x, true))
    } else {
        buf.trim().parse().map_err(serde::de::Error::custom).map(|x| (x, false))
    }
}
const fn decoupled_default() -> bool {
    false
}
const fn flags_default() -> SongBuilderFlags {
    SongBuilderFlags::empty()
}
const fn vcrange_default() -> RangeInclusive<i8> {
    0..=15
}
#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct SoundtrackConfig {
    mainbank: Option<PathBuf>,
    #[serde(default = "decoupled_default")]
    decoupled: bool,
    #[serde(default = "flags_default")]
    flags: SongBuilderFlags,
    outputdir: PathBuf,
    dsp: DSPConfig,
    sample_rate_adjustment_curve: usize,
    pitch_adjust: i64,
    #[serde(default = "vcrange_default")]
    vcrange: RangeInclusive<i8>,
    soundfonts: IndexMap<String, PathBuf>,
    songs: IndexMap<String, SongConfig>
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

        let mut global_samples_used = HashSet::new();
        let mut global_song_usage_trackers = HashMap::new();

        for (name, song_config) in soundtrack_config.songs.iter() {
            println!("[*] Reading MIDI file {:?}", song_config.mid);
            let smf_source = std::fs::read(&song_config.mid)?;
            let smf = open_midi(&smf_source)?;

            let mut smdl = SMDL::default();
            let (song_preset_map, mut samples_used, mut instrument_mappings_used, _) = smdl.from_midi_once(&smf,
                get_file_last_modified_date_with_default(&song_config.mid)?,
                name,
                (0, 255),
                soundtrack_config.vcrange.clone(),
                &soundfonts,
                &song_config.uses)?;
            
            // Write to file
            smdl.write_to_file(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join(format!("bgm{:04}.smd", song_config.i)))?)?;
        
            if !soundtrack_config.decoupled {
                global_samples_used.extend(samples_used.get_or_insert(HashSet::new()).iter().cloned());
                global_song_usage_trackers.insert(name.clone(), (song_preset_map, samples_used, instrument_mappings_used));
            } else {
                // =========== SWDL (decoupled) ===========

                let mut swdl = SWDL::default();
                let mut sample_mapping_information: HashMap<String, (HashMap<u16, u16>, BTreeMap<u16, SampleInfo>)> = HashMap::new();
                
                for soundfont_name in &song_config.uses {
                    let soundfont_path = &soundtrack_config.soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?;
                    let sf2 = soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?;
                    let (sample_rate, sample_rate_relative) = soundtrack_config.dsp.resample_at;
                    sample_mapping_information.insert(soundfont_name.clone(), swdl.trimmed_raw_sample_copy(soundfont_name,
                        &File::open(soundfont_path)?,
                        sf2,
                        DSPOptions {
                            resample_threshold: soundtrack_config.dsp.resample_threshold.round() as u32,
                            sample_rate,
                            sample_rate_relative,
                            adpcm_encoder_lookahead: soundtrack_config.dsp.adpcm_encoder_lookahead as i32
                        },
                        soundtrack_config.sample_rate_adjustment_curve,
                        soundtrack_config.pitch_adjust,
                        &samples_used.get_or_insert(HashSet::new()))?);
                }

                swdl.from_sf2_once(&soundfonts,
                    &song_config.uses,
                    get_file_last_modified_date_with_default(&song_config.mid)?,
                    name,
                    (0, 255),
                    soundtrack_config.vcrange.clone(),
                    soundtrack_config.sample_rate_adjustment_curve,
                    soundtrack_config.pitch_adjust,
                    &song_preset_map,
                    &sample_mapping_information,
                    &instrument_mappings_used.get_or_insert(HashSet::new()),
                    &samples_used.get_or_insert(HashSet::new()))?;
                
                swdl.save(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join(format!("bgm{:04}.swd", song_config.i)))?, soundtrack_config.flags)?;
            }
        }

        // =========== MAIN BANK SWDL ===========

        if !soundtrack_config.decoupled {
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
                let (sample_rate, sample_rate_relative) = soundtrack_config.dsp.resample_at;
                sample_mapping_information.insert(soundfont_name.clone(), main_bank_swdl.trimmed_raw_sample_copy(soundfont_name,
                    &File::open(&soundtrack_config.soundfonts.get(soundfont_name).ok_or(DSEError::Invalid(format!("Soundfont with name '{}' not found!", soundfont_name)))?)?,
                    sf2,
                    DSPOptions {
                        resample_threshold: soundtrack_config.dsp.resample_threshold.round() as u32,
                        sample_rate,
                        sample_rate_relative,
                        adpcm_encoder_lookahead: soundtrack_config.dsp.adpcm_encoder_lookahead as i32
                    },
                    soundtrack_config.sample_rate_adjustment_curve,
                    soundtrack_config.pitch_adjust,
                    &global_samples_used)?);
            }

            // Write
            main_bank_swdl.save(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join("bgm.swd"))?, soundtrack_config.flags)?;

            for (name, song_config) in soundtrack_config.songs.iter() {
                // =========== SWDL ===========

                let mut swdl = SWDL::default();

                if let Some((song_preset_map, mut samples_used, mut instrument_mappings_used)) = global_song_usage_trackers.remove(name) {
                    swdl.from_sf2_once(&soundfonts,
                        &song_config.uses,
                        get_file_last_modified_date_with_default(&song_config.mid)?,
                        name,
                        (0, 255),
                        soundtrack_config.vcrange.clone(),
                        soundtrack_config.sample_rate_adjustment_curve,
                        soundtrack_config.pitch_adjust,
                        &song_preset_map,
                        &sample_mapping_information,
                        &instrument_mappings_used.get_or_insert(HashSet::new()),
                        &samples_used.get_or_insert(HashSet::new()))?;
                } else {
                    panic!("{}Internal usage tracking structs missing for song '{}'!", "Internal Error: ".red(), name);
                }
                
                swdl.save(&mut open_file_overwrite_rw(soundtrack_config.outputdir.join(format!("bgm{:04}.swd", song_config.i)))?, soundtrack_config.flags)?;
            }
        }
    } else {
        println!("{}Failed to find soundtrack.yml file!", "Error: ".red());
    }
    Ok(())
}
