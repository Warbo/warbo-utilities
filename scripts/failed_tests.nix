{ bash, wrap }:

wrap {
  name   = "failed_tests";
  paths  = [ bash ];
  script = ''
    #!/usr/bin/env bash
    set -e
    shopt -s nullglob

    resultDir="/tmp/test_results.individual"

    [[ -d "$resultDir" ]] || {
      echo "WARNING: Couldn't find failures since '$resultDir' isn't a dir" 1>&2
      exit 0
    }

    pushd "$resultDir" > /dev/null
      grep -R 'FAIL' . | cut -d ':' -f1 | sed -e 's@^./@@g'
    popd > /dev/null
    exit 0
  '';
}
