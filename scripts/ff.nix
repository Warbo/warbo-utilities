{ bash, coreutils, dbus, fail, firefox, python, wrap, writeScript, xdotool,
  xvfb-run-safe }:

with rec {
  runFF = wrap {
    name = "runFF";
    paths  = [ bash (dbus.tools or dbus) fail firefox python xdotool ];
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

      msg = sys.stderr.write
      def fail(s):
        raise Exception(s + '\n')

      # Gather arguments

      url, out = map(os.getenv, ['URL', 'DIR'])

      if url is None:
        fail('No URL')
      if out is None:
        fail('No DIR')
      if not os.path.isdir(out):
        fail('Directory "' + out + '" not found')

      # Create working environmsnt

      ff_dir = out + '/firefox-profile'
      home   = out + '/home'

      os.makedirs(ff_dir)
      os.makedirs(home)

      shutil.copy(os.getenv('ffSettings'), ff_dir + '/user.js')

      msg('\nLaunching Firefox\n')

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
          fail('\n'.join(["".join(stdout_buff),
                          "".join(stderr_buff),
                          'Firefox died']))

      def xdo(args):
        assertAlive()
        subprocess.check_call(['xdotool'] + args)

      def sleep(n):
        for _ in range(n):
          assertAlive()
          msg('.')
          time.sleep(1)

      msg('Waiting for Firefox window to appear')
      extra = os.getenv('FF_EXTRA_CODE')
      if extra is not None:
        msg('Running ' + extra + '\n')
        subprocess.check_call([extra])

      msg('\nOpening Web console')
      xdo(['key', 'ctrl+shift+K'])
      sleep(10)

      msg('\nExtracting body HTML')
      xdo(['type',
           'window.dump("PRE" + document.documentElement.outerHTML + "POST");'])
      sleep(3)
      xdo(['key', '--clearmodifiers', 'Return'])
      sleep(2)

      msg('\nReading output')

      stdout_alive[0] = False
      stderr_alive[0] = False
      stdout_thread.join()
      stderr_thread.join()

      msg('\nClosing Firefox')
      handle.terminate()
      handle.wait()

      msg('\nExtracting HTML\n')

      out = "".join(stdout_buff)
      err = "".join(stderr_buff)
      got = None

      if 'PRE' in out and 'POST' in out:
        got = out
      if 'PRE' in err and 'POST' in err:
        got = err

      if got is None:
        fail('\n'.join([out, err, 'No PRE/POST sentinels found']))

      print(got.split('PRE')[1].split('POST')[0])
      msg('\nDone\n')
    '';
  };

  ff = wrap {
    name  = "firefox-runner";
    paths = [ bash coreutils ];
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
  paths  = [ bash ];
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
