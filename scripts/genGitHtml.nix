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

      cleaner = with { py = python3.withPackages (p: [ p.bleach ]); };
        wrap {
          name = "cleaner.py";
          paths = [ py ];
          script = ''
            #!${py}/bin/python3
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

    mv html "$out"
  '';
};
withDeps [ test ] script
