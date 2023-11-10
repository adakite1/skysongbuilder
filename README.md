# skysongbuilder
SkySongBuilder is a tool to build custom songs for Pokémon Mystery Dungeon: Explorers of Sky from various formats, including:
- In-engine music from Soundfont2 and MIDI files
- Streamed music from raw audio files (wav, mp3, etc.)

## Features:
- **Optimizations down to the note-level for in-engine songs**<br/>
Any Soundfonts you put in are optimized and trimmed down, all the way to the "what notes are played in the MIDI files?" level. For example, if you have a piano soundfont with 352 samples
with 4 whole velocity layers and samples for all 88 keys, but a song keeps playing the note E at a velocity of 80 over and over and over again, the converted sample bank will only contain 1 sample: the E at a velocity of 80. From there, every level of a soundfont is optimized to strip away anything that isn't strictly required.

- **4-bit ADPCM compression and loop-preservation for raw audio songs**<br/>
The same processing used for importing samples seamlessly is used for raw audio songs to compress them 4-fold from 16-bit PCM while tracking and preserving loop-points.

- **Simple configuration**<br/>
You just have to make one configuration file named `soundtrack.yml`. Here's an [example](https://github.com/adakite1/skysongbuilder/blob/master/soundtrack.yml) of what that might look like.

## How to use:
1. Modify the example configuration file to taste.
2. Download the most recent version of SkySongBuilder here: [Releases](https://github.com/adakite1/skysongbuilder/releases)
3. Run it inside the same folder as the configuration file
  - It is recommended to run it inside PowerShell on Windows or Terminal on macOS and Linux so you can see what is happening:
    - **Windows 10/11:** See [this tutorial](https://www.youtube.com/watch?v=bgSSJQolR0E). Once you have the command prompt or PowerShell open inside the folder where you have the exe, type `./skysongbuilder.exe` and hit enter.
    - **macOS:** Open the `Terminal` app, and then type `cd ` (with a space at the end) (do not hit enter!). Drag the folder containing the configuration file and the executable into the Terminal window, this will automatically fill in the path to that folder into the terminal window. Now hit enter to change directories into the directory. Then type `./skysongbuilder` and hit enter.
    - **Linux:** You know the drill.
4. You're done!

**For more information, make sure to peruse these resources!**<br/>
See the [FAQ](https://github.com/adakite1/skysongbuilder/wiki/FAQ) for answers to a list of common questions and issues.<br/>
See the [example configuration](https://github.com/adakite1/skysongbuilder/blob/master/soundtrack.yml) for a reference of all the options provided in the latest version of SkySongBuilder.
