{ bashInteractive, expect, fold-unbuffered, nix-helpers, python, raw, runCommand,
  utillinux, withDeps, wrap }:

with builtins;
with rec {
  go = wrap {
    name  = "wrappedShell";
    file  = raw.wrappedShell;
    paths = [ bashInteractive expect nix-helpers.fail ];
    vars  = {
      fold          = fold-unbuffered;
      wrappedExpect = raw."wrappedShell.expect";
    };
  };

  check = name: script: runCommand "wrappedShell-${name}"
    {
      inherit go script;
      buildInputs = [ expect nix-helpers.fail ];
    }
    ''
      ln -s "$go" ./go
      echo "$script" | expect || fail "Fail"
      mkdir "$out"
    '';

  checks = {
    immediatePrompt = check "immediate-prompt" ''
      spawn ./go -i
      set timeout 5
      expect {
        "$"     { puts "Found prompt"; exit 0; }
        timeout { puts "Timed out";    exit 1; }
      }
    '';

    canEcho = check "can-echo" ''
      spawn ./go -i
      set timeout 5
      expect {
        "$"     { puts "Found prompt";                 }
        timeout { puts "Timed out for prompt"; exit 1; }
      }
      expect *
      sleep 1
      send -- ""
      expect *
      sleep 1
      send -- "echo hello | sed -e 's/l/L/g'\n"
      sleep 1
      expect {
        "heLLo" { puts "Found result";         exit 0; }
        timeout { puts "Timed out for result"; exit 1; }
      }
    '';
  };
};
withDeps (attrValues checks) go
