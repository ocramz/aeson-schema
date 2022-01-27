{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring
      , containers, directory, fail, filepath, ghc-prim, hashable, hint
      , HUnit, lib, mtl, QuickCheck, regex-base, regex-compat, regex-tdfa
      , scientific, syb, template-haskell, temporary, test-framework
      , test-framework-hunit, test-framework-quickcheck2, text, th-lift
      , transformers, unordered-containers, vector
      }:
      mkDerivation {
        pname = "aeson-schema";
        version = "0.4.2.0";
        src = ./.;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          aeson attoparsec base bytestring containers fail ghc-prim mtl
          QuickCheck regex-base regex-compat regex-tdfa scientific syb
          template-haskell text th-lift transformers unordered-containers
          vector
        ];
        testHaskellDepends = [
          aeson attoparsec base bytestring containers directory filepath
          hashable hint HUnit mtl QuickCheck regex-compat scientific
          template-haskell temporary test-framework test-framework-hunit
          test-framework-quickcheck2 text unordered-containers vector
        ];
        homepage = "https://github.com/ocramz/aeson-schema";
        description = "Haskell JSON schema validator and parser generator";
        license = lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
