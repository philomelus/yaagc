
(load "yaagc.asd")
(load "yaagc-tests.asd")

(ql:quickload "yaagc-tests")

(in-package :yaagc-tests)

(uiop:quit (if (run-all-tests) 0 1))

