{ bash, coreutils, dbus, fail, firefox, python, wrap, writeScript, xdotool,
  xvfb-run-safe }:

with rec {
  runFF = wrap {
    name = "runFF";
    paths  = [ bash (dbus.tools or dbus) fail firefox
               (python.withPackages (p: [ p.xlib ]))
               xdotool ];
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
      import Xlib.display

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

      debug = os.getenv('DEBUG') is not None

      # Create working environmsnt

      ff_dir = out + '/firefox-profile'
      home   = out + '/home'

      os.makedirs(ff_dir)
      os.makedirs(home)

      shutil.copy(os.getenv('ffSettings'), ff_dir + '/user.js')

      def currentWindows():
        return Xlib.display.Display().screen().root.query_tree().children

      priorWindowCount = len(currentWindows())

      msg('\nLaunching Firefox\n')

      handle = subprocess.Popen(
        ['dbus-launch', 'firefox', '-profile', ff_dir, '-no-remote',
          '-new-instance', url],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        env={
          'HOME':       home,
          'PATH':       os.getenv('PATH'),
          'DISPLAY':    os.getenv('DISPLAY'),
          'XAUTHORITY': os.getenv('XAUTHORITY')
        })

      # Read output, using threads to ensure pipes get flushed

      def readInThread(h, buffer, stayAlive):
        # Keep reading until the main thread toggles stayAlive[0]
        while True:
          # We shouldn't read unless there's at least one byte available. We
          # check for this using select, with a 1 second timeout.
          r, _, _ = select.select([h], [], [], 1)

          # If data is available to be read from h, read a chunk
          if h in r:
            buffer.append(os.read(h.fileno(), 1024))
          elif not stayAlive[0]:
            # Nothing left, and we've been told to die
            break

          # Wait to avoid thrashing the CPU
          time.sleep(0.1)
        h.close()

      def readFrom(h):
        buffer    = []
        stayAlive = [True]
        thread    = threading.Thread(
          target=readInThread,
          args=(h, buffer, stayAlive))

        def stopReading():
          stayAlive[0] = False
          thread.join()
          return "".join(buffer)

        thread.start()
        return stopReading

      getStdout = readFrom(handle.stdout)
      getStderr = readFrom(handle.stderr)

      # Control Firefox

      def fail(s):
        # Force threads to die
        getStdout()
        getStderr()

        # Force Firefox to die
        try:
          handle.terminate()
        except OSError:
          pass
        handle.wait()

        raise Exception(s + '\n')

      def stillAlive():
        return handle.poll() is None

      def assertAlive():
        if not stillAlive():
          fail(repr({
            'stdout': getStdout(),
            'stderr': getStderr(),
            'error' : 'Firefox died'
          }))

      def xdo(args):
        assertAlive()
        subprocess.check_output(['xdotool'] + args,
                                stderr=None if debug else subprocess.STDOUT)

      def sleep(n):
        for _ in range(n):
          assertAlive()
          msg('.')
          time.sleep(1)

      msg('Waiting for Firefox window to appear')
      for _ in range(120):  # Give a long timeout just in case
        if len(currentWindows()) > priorWindowCount:
          break  # Use Xlib to break out early if we see a window
        sleep(1)

      if len(currentWindows()) > priorWindowCount:
        # Give FF some time to sort itself out
        sleep(25)
      else:
        fail("No Firefox window found")

      # If FF_EXTRA_CODE is given, it's a script which can do arbitrary stuff
      # before we grab the HTML.
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
      sleep(5)

      msg('\nReading output')
      out = getStdout()
      err = getStderr()

      msg('\nClosing Firefox')
      handle.terminate()
      handle.wait()

      msg('\nExtracting HTML\n')

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
  paths  = [ bash coreutils ];
  vars   = {
    inherit ff;
    xvfb = xvfb-run-safe;
  };
  script = ''
    #!/usr/bin/env bash
    set -e
    [[ -z "$DEBUG" ]] || set -x

    export URL="$1"

    # shellcheck disable=SC2154
    if [[ -n "$EXISTING_DISPLAY" ]]
    then
      timeout 300 "$ff"
    else
      timeout 300 "$xvfb" "$ff"
    fi
  '';
}
