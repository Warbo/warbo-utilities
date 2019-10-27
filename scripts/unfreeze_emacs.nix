{ checkedRacket, wrap, xclip }:

wrap {
  name   = "unfreeze_emacs";
  paths  = [ checkedRacket xclip ];
  script = ''
    #!${checkedRacket}/bin/racket
    #lang racket

    (require shell/pipeline)

    (run-pipeline '(echo "(setf debug-on-quit nil)")
                  '(xclip))

    (run-pipeline '(killall -s USR2 emacs))
  '';
}
