code:
  nix develop -c -- $EDITOR .

watch:
  nix develop -c -- spago build --watch --clear-screen

repl:
  nix develop -c -- spago repl

clean:
  rm -rf dce-output/ output/ dist/

build:
  nix develop -c -- spago build

nix-generate:
  nix develop -c -- spago2nix generate
  nix develop -c -- node2nix-hs
