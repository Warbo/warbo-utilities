{ artemis, bash, fail, git, git2html, mhonarc, pandoc, python3, raw, runCommand
, withDeps, wrap, writeScript }:

with rec {
  script = wrap {
    name = "genGitHtml";
    paths = [ fail git git2html mhonarc pandoc ];
    file = raw."genGitHtml.sh";
    vars = {
      splicer = wrap {
        name = "splicer";
        paths = [ python3 ];
        script = ''
          #!${python3}/bin/python3
          import os
          import sys
          pre, post = sys.stdin.read().split('READMESENTINEL')
          print(pre)
          print(open(os.getenv('READMEFILE'), 'r').read())
          print(post)
        '';
      };

      cleaner = wrap {
        name = "cleaner.py";
        script = ''
          #!${python3.withPackages (p: [ p.bleach ])}/bin/python3
          import bleach
          import sys

          print(bleach.clean(
            sys.stdin.read(),
            tags=['a', 'b', 'i', 'emph', 'strong', 'h1', 'h2', 'h3', 'h4',
                  'img', 'p', 'code', 'pre', 'span', 'div', 'ul', 'ol', 'li'],
            attributes={
              'a'   : ['href', 'rel'],
              'img' : ['alt',  'src'],
            }))
        '';
      };
    };
  };

  test = runCommand "genGitHtml-test" {
    inherit script;
    buildInputs = [ artemis fail git git2html mhonarc pandoc ];
    EDITOR = wrap {
      name = "test-editor";
      script = ''
        #!${bash}/bin/bash
        sed -i -e "s@^Subject: .*@Subject: $SUBJECT@g" "$1"
        sed -i -e "s@Detailed description.@$BODY@g"    "$1"
      '';
    };
    testReadme = ''
      # Title 1 #

      Some text.

      ## Title 2 ##

      A [link](http://example.org).

      <script type="text/javascript">alert("XSS");</script>

      <pre><code>Some code</code></pre>

      <ul> <li>First</li> <li>Second</li> </ul>

      <span />
    '';
  } ''
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

    for CODE in '<code>Some' '<span' '<ul>' '<li>'
    do
      grep "$CODE" < html/index.html || fail "HTML '$CODE' didn't survive"
    done
    < html/index.html | grep -v 'cloudflare' | grep '<script' &&
      fail "Should've stripped '<script>' tag"

    mv html "$out"
  '';
};
withDeps [ test ] script
