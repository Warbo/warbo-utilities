#!/usr/bin/env racket
#lang racket

(require shell/pipeline)

(module+ test
  (require rackunit))

(define open-urls
  (string-split (run-pipeline/out '(get_conkeror_urls))
                "\n"))

(define pids
  (map string-trim
       (string-split (run-pipeline/out '(ps auxww)
                                       '(grep conkeror)
                                       '(grep -v grep)
                                       '(sed -e "s/ [ ]*/ /g")
                                       '(cut -d " " -f 2))
                     "\n")))

(for-each (lambda (pid)
            (run-pipeline `(kill ,pid)))
          pids)
(sleep 3)
(run-pipeline '(sh -c "(conkeror &)"))
(sleep 3)

(for-each (lambda (url)
            (run-pipeline `(conkeror ,url)))
          open-urls)
