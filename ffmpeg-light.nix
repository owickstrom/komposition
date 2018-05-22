{ mkDerivation, base, bytestring, either, exceptions, fetchgit
, ffmpeg, JuicyPixels, libavcodec, libavdevice, libavformat
, libswscale, mtl, stdenv, transformers, vector
}:
mkDerivation {
  pname = "ffmpeg-light";
  version = "0.12.1.0";
  src = fetchgit {
    url = "https://github.com/acowley/ffmpeg-light";
    sha256 = "0g5m4kvq54brxhc3clfblfvmmm7pjcwkg4np4ww54d94kfgdw1hw";
    rev = "b40273a6bca9cb810b9a4daa70f378efdda24e59";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring either exceptions JuicyPixels mtl transformers
    vector
  ];
  libraryPkgconfigDepends = [
    ffmpeg libavcodec libavdevice libavformat libswscale
  ];
  homepage = "http://github.com/acowley/ffmpeg-light";
  description = "Minimal bindings to the FFmpeg library";
  license = stdenv.lib.licenses.bsd3;
}
