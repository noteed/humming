let

  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;

  # Overlays let us override certain packages at a central location.
  nixpkgs = import sources.nixpkgs { };
  nixpkgs-overlayed = import sources.nixpkgs { inherit overlays; };
  hp = nixpkgs-overlayed.haskellPackages;

  contents = import ./nix/contents.nix { inherit nixpkgs; };

  tooling =
    [
      # Niv is great at pinning dependencies in sources.json and computing SHA's etc.
      (hp.callCabal2nix "niv" sources.niv { })

      # Haskell tools
      hp.cabal-install
      hp.ghcid
      hp.hlint
      hp.hasktags
      hp.fourmolu
      hp.apply-refact

      # Add more as we need them.
      nixpkgs.treefmt

      # PostgreSQL
      nixpkgs.postgresql
    ];

in hp.shellFor {
  packages = p: map (contents.getPkg p) (builtins.attrNames contents.pkgList);
  buildInputs = tooling;
}
