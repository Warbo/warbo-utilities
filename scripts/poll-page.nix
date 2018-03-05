# Keep sending a given snippet of Javascript to a headless chromium repl,
# waiting one second in between, until the stdout satisfies some given condition
{ chromium-exec, fetchurl, python, pythonPackages, runCommand, withDeps, wrap }:

with builtins;
with rec {
  f = name: version: bits: sha256: prop: pythonPackages.buildPythonPackage {
    inherit name version;
    src = fetchurl {
      inherit sha256;
      url = concatStringsSep "" (["https://pypi.python.org/packages/"] ++ bits);
    };
    propagatedBuildInputs = prop;
  };

  wc = f "websocket-client" "0.47.0"
         ["c9/bb/8d3dd9063cfe0cd5d03fe6a1f74ddd948f384e9c1eff0eb978f3976a7d27/"
          "websocket_client-0.47.0.tar.gz"]
         "0jb1446053ryp5p25wsr1hjfdzwfm04a6f3pzpcb63bfz96xqlx4"
         [ pythonPackages.six ];

  pychrome = f "pychrome" "0.2.2"
               ["d2/38"
                "9a71c63eebfee8e9e133f9a3ae648fd88b082504c5ef09e0a1bc462742b7"
                "/pychrome-0.2.2.tar.gz"]
               "16ybsk7lm0lmj42pgisp9wgfnssw0dhq8gsarz8k3zmlsd4dzmmr"
               [ pythonPackages.python pythonPackages.requests wc pythonPackages.click ];

  controller = wrap {
    name   = "poll-page";
    paths  = [ python ];
    vars   = { chromium = "/usr/bin/chromium"; };
    script = ''
      #!/usr/bin/env python
      from contextlib import closing
      import fcntl
      import os
      import socket
      import subprocess
      import sys
      import time

      # Gather inputs

      chromium  = os.getenv('chromium')
      condition = os.getenv('CONDITION')
      snippet   = os.getenv('SNIPPET')

      assert chromium  is not None, "No chromium found"
      assert condition is not None, "No CONDITION found"
      assert snippet   is not None, "No SNIPPET found"



      # Bind to port 0, so the OS will pick a free port for us. We close the
      # socket and return this port number. Racy, but meh.

      port = None
      with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(("localhost", 0))
        port = s.getsockname()[1]



      def request(tool, destination=None, **kw):
        # Send a command via socket to 'DevToolsService' or 'V8Debugger'
        j = json.dumps(kw)
        request = 'Content-Length:%d\r\nTool:%s\r\n' % (len(j), tool)
        if destination:
          request += 'Destination:%s\r\n' % (destination,)
        request += '\r\n%s\r\n' % (j,)
        sock.send(request)
        if kw.get('command', "") not in RESPONSELESS_COMMANDS:
          time.sleep(.1)
          response = sock.recv(30000)
          if response.strip():
            j = response.split('\r\n\r\n', 1)[1]
            return json.loads(j)

        proc = subprocess.Popen('"%s" --remote-shell-port=9222' % my_paths.chrome_exe)
        RESPONSELESS_COMMANDS = ['evaluate_javascript']
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect(('localhost', 9222))
        sock.send('ChromeDevToolsHandshake\r\n')
        result = sock.recv(1024)
        print 'ping: ', request('DevToolsService', command='ping')
        time.sleep(4)
        print 'list_tabs: ', request('DevToolsService', command='list_tabs')
        request('V8Debugger', command='evaluate_javascript',
                data='javascript:window.location.reload()')
        sock.close()
        print 'done'




      # Establish socket for chromium to communicate on

      #sockfile = '/tmp/poll-page-{}.socket'.format(str(os.getpid()))

      #if os.path.exists(sockfile):
      #  os.remove(sockfile)

      #fhandle = open(sockfile, 'a')
      #fhandle.close()

      sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
      #sock.bind(sockfile)

      #def finished(output):
      #  p = subprocess.Popen([condition], stdin=subprocess.PIPE, stdout=None)
      #  p.stdin.write(output)
      #  p.stdin.close()
      #  return p.wait() == 0

      # Launch chromium with our socket

      fixed = [chromium, '--headless', '--disable-gpu', '--no-sandbox',
               #'--remote-debugging-socket-fd=' + str(sock.fileno())]
               '--remote-debugging-port=' + str(port)]
      cmd   = fixed + sys.argv[1:]

      sys.stderr.write('Running {}\n'.format(repr(cmd)))

      p   = subprocess.Popen(cmd,
                             stdin=None, #subprocess.PIPE,
                             stdout=None) #subprocess.PIPE)

      # Wait for chromium to connect to our socket
      time.sleep(10)
      sys.stderr.write('Waiting to connect')
      sock.connect(sockfile)
      sys.stderr.write('Connected\n')

      while True:
        sys.stderr.write('.')
        if p.poll() is not None:
          sys.stderr.write('Chromium died\n')
          sock.close()
          p.terminate()
          p.wait()
          sys.exit(1)
        try:
          #sock.listen(1)
          #conn, addr = sock.accept()
          #conn.setblocking(0)
          s.send(b'Hello, world')
          data = s.recv(1024)
          s.close()
          print('Received ' + repr(data))
          break
        except IOError:
          sys.stderr.write('IOError\n')
          break
          time.sleep(1)

      sys.exit(1)

      while True:
        conn.send(snippet + '\n')
        time.sleep(1)

        response = ""
        try:
          while True:
            response += conn.recv(1024)
            sys.stderr.write("RESPONSE: {}\n".format(response))
        except IOError:
          pass

        if finished(output):
          break

      p.terminate()
      p.wait()
      print(output)
    '';
  };

  go = wrap {
    name   = "poll-page-launcher";
    vars   = {
      inherit controller;
      ce = chromium-exec;
    };
    script = ''
      #!/usr/bin/env bash
      exec "$ce" "$controller" "$@"
    '';
  };

  test =
    with rec {
      iframes  = ''document.getElementsByTagName("iframe")'';
      mkString = x: ''${x}.join("\n")'';
      mapElems = f: x: "Array.prototype.slice(${x})).map(${f})";
      getSrc   = ''function(iframe) { return iframe.getAttribute("src"); }'';
      SNIPPET  = mkString (mapElems getSrc iframes);
    };
    runCommand "poll-page-test"
      {
        inherit go SNIPPET;
        page = "WwW-skstream-net-Big-Buck-Bunny-DVDRiP-FRENCH-avi-mp4/91mtjtf4";
        CONDITION = wrap {
          name   = "test-condition";
          script = ''
            #!/usr/bin/env bash
            GOT=$(cat)
            echo "GOT: $GOT" 1>&2
            X=$(( RANDOM % 5 ))
            exit "$X"
          '';
        };
    }
    ''
      GOT=$("$go" "http://www.alluc.ee/l/$page")
      echo "TEST OUT: $GOT"
      exit 1
    '';
};

#pychrome
withDeps [ test ] go
