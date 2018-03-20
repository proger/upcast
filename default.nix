{ mkDerivation, base, bytestring, conduit, conduit-extra
, containers, directory, exceptions, filepath, mtl, natural-sort
, optparse-applicative, process, random, resourcet, semigroups
, stdenv, text, time, unix, unordered-containers, vk-posix-pty
}:
mkDerivation {
  pname = "upcast";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring conduit conduit-extra containers directory
    exceptions filepath mtl natural-sort process random resourcet
    semigroups text time unix unordered-containers vk-posix-pty
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  homepage = "https://github.com/proger/upcast#readme";
  description = "Nix remote building tools";
  license = stdenv.lib.licenses.mit;
}
