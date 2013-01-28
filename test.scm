;;;
;;; Test net_punycode
;;;

(use gauche.test)

(test-start "net_punycode")
(use net_punycode)
(test-module 'net_punycode)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-net_punycode" "net_punycode is working"
       (test-net_punycode))

;; If you don't want `gosh' to exit with nonzero status even if
;; test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)




