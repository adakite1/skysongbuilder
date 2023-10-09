use std::{fs::File, io::Read, path::PathBuf, collections::{HashMap, HashSet, BTreeMap}, ops::RangeInclusive};

use colored::Colorize;
use dse::{dtype::{DSEError, ReadWrite}, swdl::{SWDL, SongBuilderFlags, sf2::DSPOptions, SampleInfo}, smdl::{midi::open_midi, SMDL}, opinionated_translators::sf2midi::{FromMIDIOnce, TrimmedSampleDataCopy, FromSF2Once}};
use fileutils::get_file_last_modified_date_with_default;
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
