{ mkShell
, nodejs-18_x
, dhall
, node2nix-hs
, easy-ps
, pname
, version
}:

mkShell {
  name = "${pname}-frontend-shell-${version}";
  buildInputs = [
    easy-ps.purs
    easy-ps.pulp
    easy-ps.psc-package
    easy-ps.purp
    easy-ps.spago
    easy-ps.psa
    easy-ps.spago2nix
    easy-ps.pscid
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
    easy-ps.purty
    easy-ps.purs-backend-es
    nodejs-18_x
    dhall
    node2nix-hs
  ];
  shellHook = ''
    source <(spago --bash-completion-script `which spago`)
    source <(node --completion-bash)
  '';
}
