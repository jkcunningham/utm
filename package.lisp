;;;; package.lisp

(defpackage #:utm
  (:use #:cl)
  (:export #:lat-lon-to-utm
		   #:utm-to-lat-lon
		   #:ellipsoid-names
           #:deg-min-sec-to-decimal
           #:decimal-to-deg-min-sec
		   ))

