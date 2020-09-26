# VBF archive tools

[![GitHub CI](https://github.com/michivi/vbf-fs/workflows/CI/badge.svg)](https://github.com/michivi/vbf-fs/actions)

This is the home to various tools for VBF files handling. VBF files are archive found in various video games such as the Final Fantasy X HD Remaster on PC. It contains the game's assets, such as graphics, sounds and videos.

This project consists of a library that can be used in other projects as well as several example executables.

## VBF command line program

A CLI program that offers several commands to explore the content of a VBF archive.

## VBF file system

A FUSE program that can be used on Unix-like OS to mount an archive as a regular (read-only) file-system.

## Development

Use the command `nix-shell` for a dedicated development shell. Using the `stack` command directly is another option.
