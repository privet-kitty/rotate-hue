(defpackage rotate-hue/test
  (:use :cl :rotate-hue :fiveam)
  (:export #:main-suite))

(in-package :rotate-hue/test)

;; NOTE: To run this test file, execute `(asdf:test-system :rotate-hue)' in your Lisp.

(def-suite main-suite)
(in-suite main-suite)

(test basic-test
  (is (equal '(0 60 85)
             (multiple-value-list (rotate-hue::mhvc-to-qrgb-within-gamut 27.73171423268842d0 2.2954823910671998d0 5.2003855443403415d0)))))
