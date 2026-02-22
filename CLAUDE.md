# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ai-rangler is a Racket package (version 0.0, early development). Dual-licensed Apache-2.0 OR MIT.

## Build & Development Commands

```bash
# Install package and dependencies (from project root)
raco pkg install

# Compile and check dependency correctness
raco setup --check-pkg-deps --unused-pkg-deps ai-rangler

# Run all tests
raco test -x -p ai-rangler

# Run a single test file
raco test main.rkt

# View generated documentation
raco docs ai-rangler
```

## Architecture

**Language**: Racket (`#lang racket/base`). Uses Racket's standard package layout.

**Key files**:
- `info.rkt` — Package metadata, dependencies, and version (semantic versioning)
- `main.rkt` — Primary module with inline tests and CLI entry point
- `scribblings/ai-rangler.scrbl` — Scribble documentation source

**Racket module conventions**:
- `module+ test` — Test submodule (runs via `raco test`, not when required by other modules)
- `module+ main` — CLI entry point (runs via `racket main.rkt`, not when required)
- Tests use RackUnit (`rackunit` library)

**Dependencies** (defined in `info.rkt`):
- Runtime: `base`
- Build/test: `scribble-lib`, `racket-doc`, `rackunit-lib`

## CI

GitHub Actions runs on push/PR against Racket `stable` and `current` versions, both BC and CS variants. Current version builds are experimental (allowed to fail).
