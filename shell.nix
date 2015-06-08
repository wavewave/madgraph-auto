with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, bytestring, devadmin, directory
             , filepath, hashable, hslogger, HStringTemplate, LHE-sanitizer, mtl
             , process, stdenv, text, transformers, unix, unordered-containers
             , webdav-manager
             }:
             mkDerivation {
               pname = "madgraph-auto";
               version = "0.999";
               src = ./.;
               buildDepends = [
                 aeson base bytestring devadmin directory filepath hashable hslogger
                 HStringTemplate LHE-sanitizer mtl process text transformers unix
                 unordered-containers webdav-manager
               ];
               description = "automated program library for madgraph run";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
