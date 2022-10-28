{ buildDunePackage, mirage-solo5, lwt, parse-argv }:

buildDunePackage {
  pname = "mirage-bootvar-solo5";
  version = "0.6.0";

  src = builtins.fetchurl {
    url = https://github.com/mirage/mirage-bootvar-solo5/releases/download/v0.6.0/mirage-bootvar-solo5-v0.6.0.tbz;
    sha256 = "0dqpd46ikjm318dsjh3cn3iqrv00sa918qd9b82d6664z02a6ghi";
  };

  propagatedBuildInputs = [ mirage-solo5 lwt parse-argv ];
}
