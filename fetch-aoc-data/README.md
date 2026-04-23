# fetch-aoc-data

**A user-friendly Advent of Code input downloader written in Haskell.**

Downloads all your personal puzzle inputs for a given year and organises them
neatly in `data/YYYY/dayXX/input.txt`.

## Features

- One-time session token setup (saved securely in `.aoc-token`)
- Automatic skipping of already-downloaded days
- Creates `data/YYYY/dayXX/` folders only for days that actually have inputs
- Proper `User-Agent` header (as requested by AoC)
- Built-in 300ms delay between requests — respects AoC's automation rules
- Handles future/unreleased days gracefully (no empty folders, clear message)
- Automatically deletes invalid/expired tokens and prompts you to refresh
- Works offline after the first run
- Minimal dependencies, pure Cabal project

## Quick Start

```bash
# Clone the repo
git clone https://github.com/roy-crippen/aoc-haskell.git
cd aoc-haskell/fetch-aoc-data

# Build and run
cabal run
```
