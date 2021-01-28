{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      containers
      xmonad
      xmonad-contrib
      X11
    ]))
  ];
}
