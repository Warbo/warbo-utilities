{ fetchurl, pythonPackages, wrap }:
with {
  python = pythonPackages.buildPythonPackage {
    name = "leveldb";
    src = fetchurl {
      url    = https://files.pythonhosted.org/packages/ec/c1/ca3b4199bd4073e6430076f1edd8061f2f548e831eeddc3cbc077ebaa0ca/leveldb-0.194.tar.gz;
      sha256 = "9c3378b3b4336cc63303e9fe5d054a337d50bafec80ac4628db19a598c0fcd38";
    };
  };
};

wrap {
  name  = "fix-leveldb";
  file  = ../raw/fix-leveldb.py;
  paths = [ python ];
}
