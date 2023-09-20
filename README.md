# skysongbuilder
A tool to build custom songs for Pokémon Mystery Dungeon: Explorers of Sky from Soundfonts and MIDI files

**Features:**
- **Optimizations down to the note-level**<br/>
Any Soundfonts you put in are optimized and trimmed down to what notes are played in the MIDI files. For example, if you have a piano soundfont with 88 samples, but a song keeps playing E E E E E E E over and over again, the converted sample bank will only contain 1 sample: the E. That's right, it's optimizations at the note-level! From there, every level of a soundfont is optimized to strip away anything that isn't strictly required.

- **Simple configuration**<br/>
You just have to make one configuration file named `soundtrack.yml`. Here's an [example](https://github.com/adakite1/skysongbuilder/blob/master/soundtrack.yml) of what that might look like.

**How to use:**
1. Make the config file
2. Download the latest version here: [Releases](https://github.com/adakite1/skysongbuilder/releases)
3. Run it (best to do inside PowerShell so you can see any errors that happen)
4. You're done!
