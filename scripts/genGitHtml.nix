{ git, nix-helpers, python, runCommand, warbo-packages, writeScript }:
with {
  inherit (nix-helpers) fail withDeps wrap;
  inherit (warbo-packages) artemis git2html mhonarc pandocPkgs;
};
with rec {
  script = wrap {
    name   = "genGitHtml";
    paths  = [ git git2html mhonarc pandocPkgs ];
    vars   = {
      splicer = wrap {
        name   = "splicer";
        paths  = [ python ];
        script = ''
          #!/usr/bin/env python
          import os
          import sys
          pre, post = sys.stdin.read().split('READMESENTINEL')
          print(pre)
          print(open(os.getenv('READMEFILE'), 'r').read())
          print(post)
        '';
      };

      cleaner = wrap {
        name   = "cleaner.py";
        paths  = [ (python.withPackages (p: [ p.bleach ])) ];
        script = ''
          #!/usr/bin/env python
          import bleach
          import sys

          print(bleach.clean(
            sys.stdin.read(),
            tags=['a', 'b', 'i', 'emph', 'strong', 'h1', 'h2', 'h3', 'h4',
                  'img', 'p'],
            attributes={
              'a'   : ['href', 'rel'],
              'img' : ['alt',  'src'],
            }))
        '';
      };
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      shopt -s nullglob

      # If we've not been given env vars, take values from commandline args
      if [[ -z "$repoPath" ]] || [[ -z "$htmlPath" ]]
      then
        # Bail out if args weren't given
        [[ "$#" -eq 2 ]] || {
          echo "Usage: genGitHtml <repo-path> <destination>"               1>&2
          echo "Alternatively, set the 'repoPath' and 'htmlPath' env vars" 1>&2
          echo "Optionally, set REPO_LINK to your base URL"                1>&2
          exit 1
        }

        repoPath="$1"
        htmlPath="$2"
      fi

      if echo "$repoPath" | grep '^/' > /dev/null
      then
        repoPath="file://$repoPath"
      fi

      repoName=$(basename "$repoPath" .git)

      # Assume we're using chriswarbo.net, unless told otherwise
      [[ -n "$REPO_LINK" ]] ||
        REPO_LINK="http://chriswarbo.net/git/$repoName.git"

      echo "Generating pages from git history" 1>&2
      mkdir -p "$htmlPath/git"

      #        Name           Source repo    Base URL        Destination
      git2html -p "$repoName" -r "$repoPath" -l "$REPO_LINK" "$htmlPath/git"

      echo "Generating pages from issue tracker" 1>&2
      mkdir "$htmlPath/issues"

      if [[ -e "$htmlPath/git/repository/.issues" ]]
      then
        mhonarc -mhpattern '^[^\.]' -outdir "$htmlPath/issues" \
                "$htmlPath"/git/repository/.issues/*/new \
                "$htmlPath"/git/repository/.issues/*/cur
      else
        # No issues, write a placeholder instead
        HTML='<html><body>No .issues</body></html>'
        echo "$HTML" > "$htmlPath/issues/threads.html"
        unset HTML
      fi

      echo "Generating index page" 1>&2

      # Defaults
      README_MSG="No README found"
      READMEFILE="$htmlPath/readme.html"
      echo '<span />' > "$READMEFILE"
      export READMEFILE

      echo "Looking for a README file" 1>&2
      for F in README.md README README.txt
      do
        if [[ -e "$htmlPath/git/repository/$F" ]]
        then
          README_MSG=$(echo -e "Contents of $F follows\\n\\n---\\n\\n")
          pandoc -f markdown -t html < "$htmlPath/git/repository/$F" \
                                     > "$READMEFILE"
        fi
      done

      echo "Sanitising README HTML (if any), to prevent XSS" 1>&2
      SANITISED="$htmlPath/readme.sanitised"
      # shellcheck disable=SC2154
      "$cleaner" < "$READMEFILE" > "$SANITISED"
      rm "$READMEFILE"
      mv "$SANITISED" "$READMEFILE"

      pushd "$htmlPath/git/repository"
        DATE=$(git log -n 1 --format=%ci)
      popd

      function render() {
      TICK='`'

      cat <<EOF
      # $repoName #

      *Last updated: $DATE*

      Upstream URL: [''${TICK}git clone $REPO_LINK''${TICK}]($REPO_LINK)

      [Repo](repo.git/)

      [View repository](git/index.html)

      [View issue tracker](issues/threads.html)

      $README_MSG

      READMESENTINEL

      EOF
      }

      render | pandoc --standalone -f markdown -o "$htmlPath/index.html.pre"

      # shellcheck disable=SC2154
      "$splicer" < "$htmlPath/index.html.pre" > "$htmlPath/index.html"

      rm "$htmlPath/index.html.pre"
      rm "$READMEFILE"

      # Kill the working tree used by git2html
      rm -rf "$htmlPath/git/repository"

      # Clone a bare repo snapshot
      git clone --bare "$repoPath" "$htmlPath/repo.git"
      pushd "$htmlPath/repo.git"
        git repack -A -d
        git update-server-info
        MATCHES=$(find objects/pack -maxdepth 1 -name '*.pack' -print -quit)
        if [[ -n "$MATCHES" ]]
        then
          cp objects/pack/*.pack .
          git unpack-objects < ./*.pack
          rm -f ./*.pack
        fi
      popd

      # Kill mhonarc's database
      rm -f "$htmlPath/issues/.mhonarc.db"
    '';
  };

  test = runCommand "genGitHtml-test"
    {
      inherit script;
      buildInputs = [ artemis fail git git2html mhonarc pandocPkgs ];
      EDITOR = writeScript "test-editor" ''
        #!/usr/bin/env bash
        sed -i -e "s@^Subject: .*@Subject: $SUBJECT@g" "$1"
        sed -i -e "s@Detailed description.@$BODY@g"    "$1"
      '';
      testReadme = ''
        # Title 1 #

        Some text.

        ## Title 2 ##

        A [link](http://example.org).

        <script type="text/javascript">alert("XSS");</script>
      '';
    }
    ''
      mkdir home
      export HOME="$PWD/home"
      git config --global user.email "you@example.com"
      git config --global user.name "Your Name"

      mkdir html
      mkdir test.git
      pushd test.git
        git init
        SUBJECT="Need a foo" BODY="For testing" git artemis add
        echo "Testing" > foo
        git add foo
        git commit -m "Added foo"

        SUBJECT="Need a bar" BODY="More testing" git artemis add
        echo "Testing"     >> foo
        echo "123"         >  bar
        echo "$testReadme" >  README.md
        git add foo bar README.md
        git commit -m "Added bar and README"
      popd

      if "$script"
      then
        fail "Should reject when no args"
      fi

      if "$script" test.git
      then
        fail "Should reject when one arg"
      fi

      "$script" test.git html

      if [[ -e test.git/repository ]]
      then
        fail "Left over repo should have been deleted"
      fi

      [[ -e html/index.html          ]] || fail "No index.html"
      [[ -e html/git/index.html      ]] || fail "No git/index.html"
      [[ -e html/issues/threads.html ]] || fail "No issues/threads.html"

      mv html "$out"
    '';
};
withDeps [ test ] script
