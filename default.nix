let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };

  nix-filter = import sources.nix-filter;

in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.humming;
    # binaries + haddock are also available as binaries.all.
    haddock = nixpkgs.haskellPackages.humming.doc;
  }
