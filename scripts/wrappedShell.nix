{ bash, expect, fold-unbuffered, nix-helpers, python, raw, runCommand,
  utillinux, withDeps, wrap }:

with builtins;
with rec {
  go = wrap {
    name  = "wrappedShell";
    file  = raw.wrappedShell;
    paths = [ bash expect nix-helpers.fail /*utillinux*/ ];
    vars  = {
      fold = fold-unbuffered;

      # For my convenience; I'll remove if it's causing people problems ;)
      TERM = "xterm-256color";
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
/*
    getPrompt = check "get-prompt" ''
      X=$(echo -e '\n\n' | "$go" -i 2>&1) || fail "Died\n$X"
      echo "GOT: <<$X>>" 1>&2
      echo "$X" | grep 'bash-[0-9.]*\$' > /dev/null ||
        fail "Didn't spot prompt in output\n$X"
      mkdir "$out"
    '';
*/

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
