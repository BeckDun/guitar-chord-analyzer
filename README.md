# guitar-chord-analyzer
A guitar chord analyzer in Haskell


## Prerequisites

- GHC (Glasgow Haskell Compiler)
- Cabal
- pkg-config
- SDL2 and SDL2_mixer (for audio playback)

### Installing Dependencies

**macOS (Homebrew):**
```bash
brew install pkg-config sdl2 sdl2_mixer
```

After installing, add this to your `~/.zshrc` or `~/.bashrc`:
```bash
export PKG_CONFIG_PATH="$(brew --prefix sdl2_mixer)/lib/pkgconfig:$(brew --prefix sdl2)/lib/pkgconfig:$PKG_CONFIG_PATH"
```

Then reload your shell:
```bash
source ~/.zshrc  # or source ~/.bashrc
```

**Ubuntu/Debian:**
```bash
sudo apt-get install pkg-config libsdl2-dev libsdl2-mixer-dev
```

**Fedora:**
```bash
sudo dnf install pkg-config SDL2-devel SDL2_mixer-devel
```

**Arch Linux:**
```bash
sudo pacman -S pkg-config sdl2 sdl2_mixer
```

**Windows:**

1. Install MSYS2 from https://www.msys2.org/
2. Open MSYS2 MinGW 64-bit terminal and run:
```bash
pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_mixer
```
3. Add the MSYS2 bin folder to your PATH (e.g., `C:\msys64\mingw64\bin`)

## Building the Project

1. Clone the repository:
```bash
git clone git@github.com:BeckDun/guitar-chord-analyzer.git
cd guitar-chord-analyzer
```

2. Update Cabal package list:
```bash
cabal update
```

3. Build the project:
```bash
cabal build
```

## Running the Project

Run the fretboard GUI:
```bash
cabal run fretboard
```

Run the music logic tests:
```bash
cabal run music-cli
```

## Audio Files

Place guitar note samples in the `audio/` directory with the following names:
- `C.wav`, `Cs.wav`, `D.wav`, `Ds.wav`, `E.wav`, `F.wav`
- `Fs.wav`, `G.wav`, `Gs.wav`, `A.wav`, `As.wav`, `B.wav`

Note: `s` indicates sharp (e.g., `Cs.wav` is C#)

## Usage

- Click on the fretboard to select fret positions
- Click the X buttons on the left to mute/unmute strings
- The chord name displays below the fretboard
- Click "Play" to hear the selected notes

## Troubleshooting

### pkg-config not found
Make sure pkg-config is installed:
```bash
# macOS
brew install pkg-config

# Ubuntu/Debian
sudo apt-get install pkg-config
```

### SDL2_mixer not found in pkg-config database (macOS)
Ensure your `PKG_CONFIG_PATH` is set correctly:
```bash
export PKG_CONFIG_PATH="$(brew --prefix sdl2_mixer)/lib/pkgconfig:$(brew --prefix sdl2)/lib/pkgconfig:$PKG_CONFIG_PATH"
```

### Audio files not found
Make sure your WAV files are in the `audio/` directory relative to where you run `cabal run fretboard`.