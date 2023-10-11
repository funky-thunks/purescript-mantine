code:
  nix develop -c -- $EDITOR .

clean:
  rm -rf dce-output/ output/ dist/

build:
  nix develop -c -- spago build

nix-generate:
  nix develop -c -- spago2nix generate
  nix develop -c -- node2nix-hs
