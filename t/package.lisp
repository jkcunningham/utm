;;;; package.lisp
;;
;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-user)
(defpackage :utm.test
  (:use :cl
        :fiveam
        :utm))

(in-package :utm.test)

(def-suite :utm)
(in-suite :utm)

(defclass utm-test-case ()
  ((latitude :initarg :latitude)
   (longitude :initarg :longitude)
   (ellipsoid :initarg :ellipsoid)
   (zone :initarg :zone)
   (easting :initarg :easting)
   (northing :initarg :northing)))

(defmethod print-object ((object utm-test-case) stream)
  (with-slots (latitude longitude ellipsoid zone easting northing) object
    (format stream "{ latitude: ~a, longitude: ~a, ellipsoid: ~a, zone: ~a easting: ~a, northing: ~a }"
            latitude longitude ellipsoid zone easting northing)))

(defparameter *utm-tolerance* 0.002)

(defparameter *test-coords*
  (list (make-instance 'utm-test-case
                       :latitude 39.9835310406d0
                       :longitude -105.2536113560d0
                       :ellipsoid "NAD83"
                       :zone 13
                       :easting 478346.59856d0
                       :northing 4425960.12177d0)
        (make-instance 'utm-test-case
                       :latitude 46.8392935284d0
                       :longitude -115.3348159790d0
                       :ellipsoid "NAD83"
                       :zone 11
                       :easting 626974.542d0
                       :northing 5188651.724d0)
        (make-instance 'utm-test-case
                       :latitude 48.2338208531d0
                       :longitude 19.3716430664d0
                       :ellipsoid "GRS80"
                       :zone 34
                       :easting 379083.606d0
                       :northing 5343570.714d0)
        (make-instance 'utm-test-case
                       :latitude 59.8723979923d0
                       :longitude 105.1062011719d0
                       :ellipsoid "GRS80"
                       :zone 48
                       :easting 505946.450d0
                       :northing 6637205.350d0)
        (make-instance 'utm-test-case
                       :latitude -29.3438753994d0
                       :longitude 130.8032226563d0
                       :ellipsoid "GRS80"
                       :zone 52
                       :easting 675064.430d0
                       :northing 6752564.671d0)
        ;; new tests (JKC)
        (make-instance 'utm-test-case   ; just west of exception area
                       :latitude  60.0d0
                       :longitude 2.9d0
                       :ellipsoid "WGS84"
                       :zone 31
                       :easting 494422.233d0
                       :northing 6651415.406d0)
        (make-instance 'utm-test-case   ; inside exception area
                       :latitude  60.0d0
                       :longitude 5.0d0
                       :ellipsoid "WGS84"
                       :zone 32
                       :easting 276980.0d0
                       :northing 6658157.0d0)
        ;; (make-instance 'utm-test-case   ; inside exception area
        ;;                :latitude  60.0d0
        ;;                :longitude 6.1d0
        ;;                :ellipsoid "WGS84"
        ;;                :zone 32
        ;;                :easting 338279.0d0
        ;;                :northing 6654957.0d0)
        (make-instance 'utm-test-case   ; east of exception area
                       :latitude  60.0d0
                       :longitude 12.1d0
                       :ellipsoid "WGS84"
                       :zone 33
                       :easting 338279.227d0
                       :northing 6654956.721d0)
        ))

(defun utm-near (a b)
  (< (abs (- a b)) *utm-tolerance*))

(test lat-lon-to-utm
  (dolist (coord *test-coords*)
    (with-slots (latitude longitude ellipsoid zone easting northing) coord
      (let* ((translated (lat-lon-to-utm latitude longitude :ellipsoid ellipsoid))
             (translated-easting (car translated))
             (translated-northing (cadr translated))
             (translated-zone (caddr translated)))
        (is-true (utm-near translated-easting easting)
                 "Could not convert ~a to UTM got easting ~a" coord translated-easting)
        (is-true (utm-near translated-northing northing)
                 "Could not convert ~a to UTM got northing ~a" coord translated-northing)
        (is-true (= zone translated-zone)
                 "Could not convert ~a to UTM got zone ~a" coord translated-zone)
        ))))


(test utm-to-lat-lon
  (dolist (coord *test-coords*)
    (with-slots (latitude longitude ellipsoid zone easting northing) coord
      (let* ((translated (utm-to-lat-lon easting northing zone :ellipsoid ellipsoid))
             (translated-latitude (car translated))
             (translated-longitude (cadr translated)))
        (is-true (utm-near translated-latitude latitude)
                 "Could not convert ~a to latitude/longitude... got latitude ~a"
                 coord translated-latitude)
        (is-true (utm-near translated-longitude longitude)
                 "Could not convert ~a to latitude/longitude... got longitude ~a"
                 coord translated-longitude)))))

(test decimal-deg-min-sec
  ;; TODO: Add some more test cases...
  (is-true (utm-near (deg-min-sec-to-decimal 20 30 40.0d0) 20.51111d0))
  (multiple-value-bind (deg min sec) (utm:decimal-to-deg-min-sec 20.5111119d0)
     (is-true (utm-near deg 20.0d0))
     (is-true (utm-near min 30.0d0))
     (is-true (utm-near sec 40.003967d0)))
  (is-true (utm-near (deg-min-sec-to-decimal  78 39 10.8d0) 78.653d0))
  (multiple-value-bind (deg min sec) (utm:decimal-to-deg-min-sec 78.653d0)
     (is-true (utm-near deg 78.0d0))
     (is-true (utm-near min 39.0d0))
     (is-true (utm-near sec 10.8d0)))
  )


;; Boundary for Norways screwed up system
;; 31V: lon: [0,3.0)    lat: [56.0,64.0)
;; 32V: lon: [3.0,12.0) lat: [56.0,64.0)
;;
;; Also, in the region around Svalbard, the four grid zones 31X (9° of
;; longitude in width), 33X (12° of longitude i;; n width), 35X (12°
;; of longitude in width), and 37X (9° of longitude in width) are
;; extended to cover what would otherwise have been covered by the
;; seven grid zones 31X to 37X. The three grid zones 32X, 34X and 36X
;; are not used.

(with-open-file (fs "utm-z32-boundary.pts" :direction :output :if-exists :supersede)
  (let (;;(fs *standard-output*)
        ;(Rwest   3.00001d0)
        (Rwest   6.00001d0)
        ;(Rwest   9.00001d0)
        (Rsouth 56.0d0)
        (Reast  12.0d0)
        (Rnorth 64.0d0)
        (dinc    0.01d0)
        (fmt "~&~14,4f ~14,4f ~4d~%"))
    (loop for lon from Rwest upto Reast by dinc
       do (apply #'format fs fmt (utm:lat-lon-to-utm Rsouth lon)))
    (loop for lat from Rsouth upto Rnorth by dinc
       do (apply #'format fs fmt (utm:lat-lon-to-utm lat Reast)))
    (loop for lon from Reast downto Rwest by dinc
       do (apply #'format fs fmt (utm:lat-lon-to-utm Rnorth lon)))
    (loop for lat from Rnorth downto Rsouth by dinc
       do (apply #'format fs fmt (utm:lat-lon-to-utm lat Rwest)))))
