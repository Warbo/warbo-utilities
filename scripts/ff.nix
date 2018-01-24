{ bash, coreutils, dbus, fail, nixpkgs1609, python, wrap, writeScript, xdotool,
  xorg, xsel, xvfb-run-safe }:

with rec {
  inherit (nixpkgs1609) firefox;

  runFF = wrap {
    name = "runFF";
    paths  = [ bash (dbus.tools or dbus) fail firefox python ];
    vars   = {
      ffSettings = writeScript "user.js" ''
        // Don't show bookmark icons
        user_pref("browser.shell.checkDefaultBrowser", false);
        user_pref("browser.search.update",             false);
        user_pref("update_notifications.enabled",      false);

        # Allow window.dump calls
        user_pref("browser.dom.window.dump.enabled",   true);

        # Reuse the same window/tab
        user_pref("browser.link.open_newwindow",       1);
      '';
    };
    script = ''
      #!/usr/bin/env python
      import os
      import select
      import shutil
      import subprocess
      import sys
      import threading
      import time

      # Helper functions

      msg = lambda s: sys.stderr.write(s + '\n')
      def fail(s):
        msg(s)
        sys.exit(1)

      # Gather arguments

      url, dir = map(os.getenv, ['URL', 'DIR'])

      if url is None:
        fail('No URL')
      if dir is None:
        fail('No DIR')
      if not os.path.isdir(dir):
        fail('Directory "' + dir + '" not found')

      # Create working environmsnt

      ff_dir = dir + '/firefox-profile'
      home   = dir + '/home'

      os.makedirs(ff_dir)
      os.makedirs(home)

      shutil.copy(os.getenv('ffSettings'), ff_dir + '/user.js')

      # Launch Firefox

      handle = subprocess.Popen(
        ['dbus-launch', 'firefox', '-profile', ff_dir, '-no-remote',
          '-new-instance', url],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        env={
          'HOME': home,
          'PATH':    os.getenv('PATH'),
          'DISPLAY':    os.getenv('DISPLAY'),
          'XAUTHORITY': os.getenv('XAUTHORITY')
        })

      # Read output, using threads to ensure pipes get flushed

      def readInThread(handle, buffer, stayAlive):
        while stayAlive[0]:
          # We shouldn't read unless there's at least one byte available. We
          # check for this using select, with a 1 second timeout.
          r, _, _ = select.select([handle], [], [], 0.1)
          if handle in r:
            buffer.append(os.read(handle.fileno(), 1024))
        handle.close()

      def readFrom(handle):
        buffer    = []
        stayAlive = [True]
        thread    = threading.Thread(
          target=readInThread,
          args=(handle, buffer, stayAlive))
        thread.start()
        return (buffer, stayAlive, thread)

      stdout_buff, stdout_alive, stdout_thread = readFrom(handle.stdout)
      stderr_buff, stderr_alive, stderr_thread = readFrom(handle.stderr)

      # Control Firefox

      def stillAlive():
        return handle.poll() is None

      def assertAlive():
        if not stillAlive():
          msg("".join(stdout_buff))
          msg("".join(stderr_buff))
          raise Exception("Firefox died")

      def xdo(args):
        assertAlive()
        subprocess.check_call(['xdotool'] + args)

      def sleep(n):
        while n > 0:
          assertAlive()
          time.sleep(1)
          n -= 1

      sleep(20)

      extra = os.getenv('FF_EXTRA_CODE')
      if extra is not None:
        subprocess.check_call([extra])

      msg("Opening Web console")
      xdo(['key', 'ctrl+shift+K'])
      sleep(10)

      msg("Extracting body HTML")
      xdo(['type',
           'window.dump("PRE" + document.documentElement.outerHTML + "POST");'])
      sleep(3)
      xdo(['key', '--clearmodifiers', 'Return'])
      sleep(2)

      # Stop reading

      stdout_alive[0] = False
      stderr_alive[0] = False
      stdout_thread.join()
      stderr_thread.join()

      # Close Firefox
      handle.terminate()
      handle.wait()

      # Extract HTML

      out = "".join(stdout_buff)
      err = "".join(stderr_buff)
      got = None

      if 'PRE' in out and 'POST' in out:
        got = out
      if 'PRE' in err and 'POST' in err:
        got = err

      if got is None:
        msg(out)
        msg(err)
        fail('No PRE/POST sentinels found')

      print(got.split('PRE')[1].split('POST')[0])
    '';
  };

  ff = wrap {
    name  = "firefox-runner";
    paths = [ bash coreutils fail xdotool xorg.xwininfo xsel ];
    vars  = { inherit runFF; };
    script = ''
      #!/usr/bin/env bash
      set -e

      # Firefox messes with the disk a lot, so put it in a throwaway dir
      DIR=$(mktemp -d -t 'ff-XXXXX')
      export DIR

      function cleanUp {
        rm -rf "$DIR"
      }

      trap cleanUp EXIT

      echo "Opening Firefox on '$URL'" 1>&2
      "$runFF"
    '';
  };
};

wrap {
  name   = "ff";
  paths  = [ bash coreutils ];
  vars   = {
    inherit ff;
    xvfb = xvfb-run-safe;
  };
  script = ''
    #!/usr/bin/env bash
    # shellcheck disable=SC2154
    URL="$1" "$xvfb" "$ff"
  '';
}
