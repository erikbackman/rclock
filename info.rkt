#lang info
(define collection "foo")
(define deps '("base" "racket-gui"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/foo.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(ebn))
(define license '(Apache-2.0 OR MIT))
