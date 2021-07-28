{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = with pkgs; [ cabal-install ];
  inputsFrom = [ (import ./default.nix { inherit pkgs; }).env ];
}
