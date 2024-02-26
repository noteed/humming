let

  sources = import ./sources.nix;
  defNixpkgs = import sources.nixpkgs { };
  nix-filter = import sources.nix-filter;

in { nixpkgs ? defNixpkgs }:

let inherit (nixpkgs.lib.attrsets) getAttrFromPath;
in {
  # Lists all packages made available through this nix project.
  # The format is `{ <pkgName> : <pkgDir> }` (we refer to this as pInfo).
  # The used directory should be the path of the directory relative to the root
  # of the project.
  pkgList = {
    humming = nix-filter {
      root = ../.;
      include = with nix-filter; [
        "humming.cabal"
        "LICENSE"
        (and "bin" (or_ (matchExt "hs") isDirectory))
        (and "Database" (or_ (matchExt "hs") isDirectory))
        (and "Humming" (or_ (matchExt "hs") isDirectory))
        (and "tests" (or_ (matchExt "hs") isDirectory))
      ];
    };
  };

  # Get an attribute from a string path from a larger attrSet
  getPkg = pkgs: pPath: getAttrFromPath [pPath] pkgs;
}
