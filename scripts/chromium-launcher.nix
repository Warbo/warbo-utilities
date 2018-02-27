{ bash, fetchurl, proot, runCommand, wrap, writeScript }:

with rec {
  rootVersion = "67a0101a76eed558d4b61a484a27c9f9d7a119f4/stretch";

  rootRepo = "debuerreotype/docker-debian-artifacts";

  rootfs = fetchurl {
    url    = "https://github.com/${rootRepo}/raw/${rootVersion}/rootfs.tar.xz";
    sha256 = "1ff2qjvfj6fbwwj17wgmn3a4mlka1xv1p3jyj465dbf4qf3x0ijm";
  };

  # See https://github.com/proot-me/PRoot/issues/106
  PROOT_NO_SECCOMP = "1";

  env = runCommand "debian-with-chromium"
    {
      inherit rootfs PROOT_NO_SECCOMP;
      buildInputs      = [ proot ];
      SSL_CERT_FILE    = /etc/ssl/certs/ca-bundle.crt;
      script           = writeScript "setup.sh" ''
        #!/usr/bin/env bash
        set -e
        apt-get update
        apt-get install -y chromium
        chmod 4755 /usr/lib/chromium/chrome-sandbox
      '';
    }
    ''
      echo "Unpacking Debian" 1>&2
      mkdir "$out"
      pushd "$out"
        tar xf "$rootfs"
      popd

      echo "Installing setup script" 1>&2
      cp "$script" "$out/setup.sh"

      echo "Pointing PATH to Debian binaries" 1>&2
      export PATH="/bin:/usr/bin:/sbin:/usr/sbin:$PATH"

      echo "Resetting /tmp variables" 1>&2
      export TMPDIR=/tmp
      export TEMPDIR=/tmp
      export TMP=/tmp
      export TEMP=/tmp

      echo "Setting up" 1>&2
      proot -r "$out" -b /proc -b /dev -0 /setup.sh
    '';

  launcher = wrap {
    name   = "chromium-launcher";
    paths  = [ bash proot ];
    vars   = {
      inherit env PROOT_NO_SECCOMP;
      launcher = writeScript "chrome-launcher.sh" ''
        #!/usr/bin/env bash
        chromium --disable-namespace-sandbox --no-sandbox "$@"
      '';
    };
    script = ''
      #!/usr/bin/env bash
      export PATH="/bin:/usr/bin:/sbin:/usr/sbin:$PATH"
      export TMPDIR=/tmp
      export TEMPDIR=/tmp
      export TMP=/tmp
      export TEMP=/tmp

      CHROME_USER_DATA_DIR=$(mktemp --tmpdir -d 'chromium-launcher-XXXXX')
      export CHROME_USER_DATA_DIR
      #chmod -R 777 "$CHROME_USER_DATA_DIR"
      function cleanup {
        rm -rf "$CHROME_USER_DATA_DIR"
      }
      trap cleanup EXIT

      proot -r "$env" -b /proc -b /dev -b /nix -b /tmp -b /home \
        "$launcher" --user-data-dir="$CHROME_USER_DATA_DIR" "$@"
    '';
  };
};
launcher
