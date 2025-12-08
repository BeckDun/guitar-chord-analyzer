# guitar-chord-analyzer
A guitar chord analyzer in Haskell. This allows you to visualize different chords on the guitar. 


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

After installing, add this to your environment:
```bash
export PKG_CONFIG_PATH="$(brew --prefix sdl2_mixer)/lib/pkgconfig:$(brew --prefix sdl2)/lib/pkgconfig:$PKG_CONFIG_PATH"
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

## Usage

- Click on the fretboard to select fret positions
- Click the X buttons on the left to mute/unmute strings
- The chord name displays below the fretboard
- Click "Play" to hear the selected notes
