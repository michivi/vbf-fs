{
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/ca207f93ad2c89e21c458b17084c747093e5be3a.tar.gz") {},
  pkgs ? import haskellNix.sources.nixpkgs-2003 haskellNix.nixpkgsArgs
}:

let
  hsPkgs = import ./default.nix { inherit pkgs; };

  ghcVersion = "8.8.4";

  targetPrefix = with pkgs.stdenv; lib.optionalString (targetPlatform != buildPlatform)
                                                      (targetPlatform.config + "-");

in
  hsPkgs.shellFor {
    packages = ps: with ps; [
      vbf-fs
    ];

    withHoogle = true;

    nativeBuildInputs =
        with pkgs; [ 
            # Various basic programs.
            buildPackages.git

            # Haskell tools.
            buildPackages.cabal-install
            buildPackages.haskellPackages.brittany
            buildPackages.haskellPackages.hpack
            buildPackages.haskellPackages.hspec-discover

            # Linters and checkers.
            buildPackages.haskellPackages.hlint

            # Miscellaneous.
            buildPackages.gv
        ];
    
    shellHook = ''
      VBF_APP=dist-newstyle/build/x86_64-linux/ghc-8.8.4/vbf-fs-0.1.0.0/x/vbf/build/vbf/vbf
      VBFFS_APP=dist-newstyle/build/x86_64-linux/ghc-8.8.4/vbf-fs-0.1.0.0/x/vbf-fs/build/vbf-fs/vbf-fs

      alias "b"="cabal new-build"
      alias "c"="hpack && cabal new-configure"
      alias "d"="cabal haddock"
      alias "f"="(find app -name '*.hs' ; find src -name '*.hs') | xargs -n1 brittany --write-mode=inplace"
      alias "l"="hlint -h .hlint.yaml ."
      alias "lr"="hlint -h .hlint.yaml --report ."
      alias "r"="$VBF_APP"
      alias "rf"="$VBFFS_APP"
    '';

    exactDeps = true;
  }
