{ fetchFromGitHub, firefox, wrap, xorg }:

# Provides a command which opens Firefox on a given URL and executes a given
# Javascript script.

# With many thanks to https://lastlog.de/blog/posts/data-mining.html

with rec {
  altPkgs = import (fetchFromGitHub {
    owner  = "qknight";
    repo   = "nixpkgs";
    rev    = "a1dd8b2a5b035b758f23584dbf212dfbf3bff67d";
    sha256 = "1zn9znsjg6hw99mshs0yjpcnh9cf2h0y5fw37hj6pfzvvxfrfp9j";
  }) {};

  py = altPkgs.python3Packages.python.withPackages (p: [
    p.virtual-display p.selenium
  ]);

  inherit (altPkgs.python3Packages.buildPythonPackage rec {
    name    = "python-selenium-env";
    version = "0.0.1";

    buildInputs = [ firefox xorg.xorgserver ];

    propagatedBuildInputs = with altPkgs.python3Packages; [
      virtual-display selenium
    ];
  }) env;
};
wrap {
  name   = "pySelenium";
  paths  = [ firefox xorg.xorgserver py ];
  script = ''
    #!${py}/bin/python3

    from selenium                       import webdriver
    from selenium                       import selenium
    from selenium.webdriver.common.keys import Keys
    from selenium.webdriver.common.by   import By
    from selenium.webdriver.support.ui  import WebDriverWait
    from selenium.webdriver.support     import expected_conditions as EC
    from pyvirtualdisplay               import Display

    import sys
    import os

    url    = os.getenv('URL')
    script = os.getenv('SCRIPT')

    if script is None:
      script = """
        var result = document.getElementsByTagName('html')[0].outerHTML;
        console.log(result);
        return result;
      """

    if url is None:
      sys.stderr.write("pySelenium needs a URL env var\n")
      sys.exit(1)

    display = Display(visible=0, size=(800, 600))
    display.start()

    ff = webdriver.Firefox()
    ff.get(url)

    sys.stderr.write("Running script...\n")
    print(ff.execute_script(script))
    sys.stderr.write("...Finished script\n")

    ff.quit()
    display.stop()
  '';
}
