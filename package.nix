{ callPackage
, stdenv
, easy-ps
, nodejs-18_x
, gitignoreSource
, pname
, version
, src
, spagoPackages    ? "${src}/spago-packages.nix"
, nodeDependencies ? "${src}/node-dependencies.nix"
}:

let spagoPkgs = callPackage spagoPackages {};
    nodeDeps = (callPackage nodeDependencies {}).nodeDependencies;
in stdenv.mkDerivation {
    inherit pname version;

    buildInputs = [
      spagoPkgs.installSpagoStyle
      spagoPkgs.buildSpagoStyle
      easy-ps.purs
      nodejs-18_x
    ];

    ENVIRONMENT = "production";
    NODE_PATH = "${nodeDeps}/lib/node_modules";

    src = gitignoreSource src;

    unpackPhase = ''
      cp -r $src/src .

      install-spago-style
    '';

    buildPhase = ''
      build-spago-style --codegen corefn,js "./src/**/*.purs"
    '';

    installPhase = ''
      mkdir -p $out
      cp -r output/. $out
    '';
  }
