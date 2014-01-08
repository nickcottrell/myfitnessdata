#lang racket
(require net/url
         racket/gui/base
         racket/sandbox)

(define (menu-file-exit-click item control)
  (exit 0))

(define (menu-help-about-click item control)
  (message-box
   "About MyFitnessData"
   "MyFitnessData 2.0.\nCopyright © 2013 Duncan Bayne.\nMyFitnessData is Free Software, released under the GNU LGPL."))

(define frame
  (new frame% [label "MyFitnessData"] [height 480] [width 640]))

(define menu
  (new menu-bar% [parent frame]))

(define menu-file
  (new menu% [parent menu] [label "&File"]))

(define menu-file-exit
  (new menu-item% [parent menu-file] [label "E&xit"] [callback menu-file-exit-click]))

(define menu-help
  (new menu% [parent menu] [label "&Help"]))

(define menu-help-about
  (new menu-item% [parent menu-help] [label "&About"] [callback menu-help-about-click]))

(send frame show #t)
