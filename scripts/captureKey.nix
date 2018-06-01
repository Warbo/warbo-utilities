{ keybinder, python, wrap }:

wrap {
  name   = "captureKey";
  paths  = [ (python.withPackages (p: [ p.pygtk keybinder ])) ];
  script = ''
    #!/usr/bin/env python
    import keybinder
    import signal
    import sys
    import time

    key = sys.argv[1]
    msg = lambda m: sys.stderr.write(m + '\n')

    def sigterm_handler(_signo, _stack_frame):
      msg('Attempting to unbind ' + key)
      keybinder.unbind(key)
      msg(key + ' binding released')
      sys.exit(0)

    signal.signal(signal.SIGTERM, sigterm_handler)

    msg('Attempting to bind key ' + key)
    assert keybinder.bind(key, lambda: None, None), 'Failed to bind ' + key
    msg(key + ' has been bound; TERM this process to unbind')
    while True:
      time.sleep(0.1)
  '';
}
