# VBF archive tools

[![GitHub CI](https://github.com/michivi/vbf-fs/workflows/CI/badge.svg)](https://github.com/michivi/vbf-fs/actions)

This is the home to various tools for VBF files handling. VBF files are archive found in various video games such as the Final Fantasy X HD Remaster on PC. It contains the game's assets, such as graphics, sounds and videos.

This project consists of a library that can be used in other projects as well as several example executables described below.

## VBF command line program

A CLI program that offers several commands to explore the content of a VBF archive, to extract its content or to build a new one.

Help is available through the `--help` flag.

```
Usage: vbf COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  dump                     Dump information on a VBF archive
  extract                  Extract an entry within the VBF archive
  tree                     Print a tree of the archive content
  pack                     Pack files into a new VBF archive
  unpack                   Unpack the archive to a folder
```

VBF archives can be fully unpacked and repacked as well. Repacking does include compression.

## VBF file system

A FUSE program that can be used on Unix-like OS to mount an archive as a regular (read-only) file-system.

The following command will mount the VBF archive `/tmp/test.vbf` to `/mnt/tmp`:

```bash
vbf-fs /tmp/test.vbf /mnt/tmp
```

To dismount the archive:

```bash
fusermount -u /mnt/tmp
```

## Development

Use the command `nix-shell` for a dedicated development shell. Another option is to use the `stack` command directly.

The project can be compiled with G.H.C. 8.6.5, 8.8.4 and 8.10.1. Haddock documentation is also available.

## Disclaimer

This project is just a personal experimentation. Feel free to contribute.
