{ checkedRacket, fetchFromGitHub, racketWithPackages, wrap, xclip }:

with {
  racket = racketWithPackages.override { racket = checkedRacket; } [
    (fetchFromGitHub {
      owner = "willghatch";
      repo = "racket-shell-pipeline";
      rev = "7ed9a75";
      sha256 = "06z5bhmvpdhy4bakh30fzha4s0xp2arjq8h9cyi65b1y18cd148x";
    })
  ];
};
wrap {
  name = "unfreeze_emacs";
  paths = [ racket xclip ];
  script = ''
    #!${racket}/bin/racket
    #lang racket
    (require shell/pipeline)

    ;; If emacs does become responsive, debug-on-quit will be set, which spawns
    ;; annoying buffers whenever C-g is pressed (which is a lot!). We put the
    ;; elisp for disabling it into the clipboard, in case we want it.
    (run-pipeline '(echo "(setf debug-on-quit nil)")
                  '(xclip))

    (run-pipeline '(killall -s USR2 emacs))
  '';
}
