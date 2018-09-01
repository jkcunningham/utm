;;;; utm.lisp
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

(in-package #:utm)

(declaim (inline deg2rad rad2deg))
(defun deg2rad (d)
  (declare (type double-float d))
  (* d (/ PI 180.0)))

(defun rad2deg (r)
  (declare (type double-float r))
  (* r (/ 180.0 pi)))

;; This hash tables stores parameters about the ellipsoid each model uses to model the earth
(defparameter *ellipsoids* (make-hash-table :test 'equal))

;; Fills the ellipsoids hash table
(defun fill-ellipsoids ()
  "Fill the hash table of supported ellipsoids."
  (setf (gethash "NAD83" *ellipsoids*) (cons 6378137.0 6356752.3142))
  (setf (gethash "WGS84" *ellipsoids*) (cons 6378137.0 6356752.3142))
  (setf (gethash "GRS80" *ellipsoids*) (cons 6378137.0 6356752.3141))
  (setf (gethash "WGS72" *ellipsoids*) (cons 6378135.0 6356750.5))
  (setf (gethash "Australian1965" *ellipsoids*) (cons 6378160.0 6356774.7))
  (setf (gethash "Krasovsky1940" *ellipsoids*) (cons 6378245.0 6356863.0))
  (setf (gethash "International1924" *ellipsoids*) (cons 6378388.0 6356911.9))
  (setf (gethash "Hayford1909" *ellipsoids*) (cons 6378388.0 6356911.9))
  (setf (gethash "Clake1880" *ellipsoids*) (cons 6378249.1 6356514.9))
  (setf (gethash "Clarke1866" *ellipsoids*) (cons 6378206.4 6356583.8))
  (setf (gethash "Airy1830" *ellipsoids*) (cons 6377563.4 6356256.9))
  (setf (gethash "Bessel1841" *ellipsoids*) (cons 6377397.2 6356079.9))
  (setf (gethash "Everest1830" *ellipsoids*) (cons 6377276.3 6356075.4)))

;; Populate the hash table
(fill-ellipsoids)

(defun ellipsoid-names ()
  "Return the names of all supported ellipsoids."
  (loop for key being the hash-keys in *ellipsoids* collect key))

;; Constants used in the equations
(defconstant E0 500000.0)
(defconstant k0 0.9996)
(defconstant N0 10000000.0)

;; This math is hideous in Lisp, but also pretty ugly in other languages.
;; I'm not going to pretend to understand exactly what's going on here.
;; If curious, look at:
;; * http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.htm
;; * http://www.uwgb.edu/dutchs/FieldMethods/UTMSystem.htm
(defun lat-lon-to-utm (lat lon &key (ellipsoid "WGS84") (zone nil))
  "Convert a point given as latitude and longitude into UTM using the specified ellipsoid.  The default ellipsoid is WGS84."
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (declare (type double-float lat lon)
           (type (or null fixnum) zone))
  (let* ((lat-rad (deg2rad lat))
         (lon-rad (deg2rad lon))
         (a (car (gethash ellipsoid *ellipsoids*)))
         (b (cdr (gethash ellipsoid *ellipsoids*)))
         (f (/ (- a b) a))
         (e-squared (* f (- 2.0 f)))
         (e-prime-squared (/ e-squared (- 1.0 e-squared)))
         (nzone (if zone zone (ceiling (/ (+ lon 180.0) 6.0))))
         (long0 (deg2rad (- (* 6.0 (- nzone 1)) 177) ))
         ;;(rho (/ (* a (- 1 e-squared)) (expt (- 1.0 (* e-squared (expt (sin lat-rad) 2.0))) (/ 3.0 2.0))))
         (nu (/ a (expt (- 1.0 (* e-squared (expt (sin lat-rad) 2.0))) 0.5)))
         (p (- lon-rad long0))

         (M (* a (+ (* (- 1.0 (/ e-squared 4.0) (* (/ 3.0 64.0) (expt e-squared 2.0)) (* (/ 5.0 256.0) (expt e-squared 3.0))) lat-rad)
                    (- (* (sin (* 2.0 lat-rad)) (+ (* (/ 3.0 8.0) e-squared) (* (/ 3.0 32.0) (expt e-squared 2.0)) (* (/ 45.0 1024.0) (expt e-squared 3.0)))))
                    (* (sin (* 4.0 lat-rad)) (+ (* (/ 15.0 256.0) (expt e-squared 2.0)) (* (/ 45.0 1024.0) (expt e-squared 3.0))))
                    (- (* (sin (* 6.0 lat-rad)) (* (/ 35.0 3072.0) (expt e-squared 3.0)))))))

         (K1 (* M k0))
         (k2 (* k0 nu (sin (* 2.0 lat-rad)) 0.250))
         (k3 (* (* k0 nu (sin lat-rad) (expt (cos lat-rad) 3.0) (/ 1.0 24.0))
                (+ 5.0 (- (expt (tan lat-rad) 2.0)) (* 9.0 e-prime-squared (expt (cos lat-rad) 2.0)) (* 4.0 (expt e-squared 2.0) (expt (cos lat-rad) 4.0)))))
         (k4 (* k0 nu (cos lat-rad)))
         (k5 (* (* k0 nu (expt (cos lat-rad) 3.0) (/ 1.0 6.0))
                (+ 1.0 (- (expt (tan lat-rad) 2.0)) (* e-prime-squared (expt (cos lat-rad) 2.0)))))
         (easting (+ E0 (* k4 p) (* k5 (expt p 3.0))))
         (northing (+ k1 (* k2 (expt p 2.0)) (* k3 (expt p 4.0)))))
    (declare (type double-float lat-rad lon-rad a b f e-squared e-prime-squared
                   long0 nu p M K1 K2 K3 k4 k5 easting northing)
             (type fixnum nzone))
    (list (realpart easting)
          (if (< lat 0 ) (+ N0 (realpart northing)) (realpart northing))
          nzone)))

;; Again, see the references for an explaination of what's going on
;; * http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.htm
;; * http://www.uwgb.edu/dutchs/FieldMethods/UTMSystem.htm
(defun utm-to-lat-lon (easting northing zone &key (ellipsoid "WGS84"))
  "Convert a point given as UTM into latitude and longitude using the specified ellipsoid.  The default ellipsoid is WGS84."
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (declare (type double-float easting northing)
           (type fixnum zone))
  (let*
      ((reasting (- easting 500000.0))
       (a (car (gethash ellipsoid *ellipsoids*)))
       (b (cdr (gethash ellipsoid *ellipsoids*)))
       (f (/ (- a b) a))
       (e-squared (* f (- 2.0 f)))
       (e-prime-squared (/ e-squared (- 1.0 e-squared)))
       (long0 (deg2rad (- (* 6.0 (- zone 1)) 177) ))
       (M (/ northing k0))
       (mu (/ M (* a (- 1.0 (/ e-squared 4.0) (* (/ 3.0 64.0) (expt e-squared 2.0)) (* (/ 5.0 256.0) (expt e-squared 3.0))))))
       (e1 (/ (- 1.0 (sqrt (- 1.0 e-squared))) (+ 1.0 (sqrt (- 1.0 e-squared)))))
       (j1 (- (* (/ 3.0 2.0) e1) (* (/ 27.0 32.0) (expt e1 3.0))))
       (j2 (- (* (/ 21.0 16.0) (expt e1 2.0)) (* (/ 55.0 32.0) (expt e1 4.0))))
       (j3 (* (/ 151 96) (expt e1 3.0)))
       (j4 (* (/ 1097.0 512.0) (expt e1 4.0)))
       (fp (+ mu
              (* j1 (sin (* 2.0 mu )))
              (* j2 (sin (* 4.0 mu )))
              (* j3 (sin (* 6.0 mu )))
              (* j4 (sin (* 8.0 mu )))))
       (c1 (* e-prime-squared (expt (cos fp) 2.0)))
       (t1 (expt (tan fp) 2.0))
       (r1 (* a (/ (- 1.0 e-squared) (expt (- 1.0 (* e-squared (expt (sin fp) 2.0))) (/ 3.0 2.0)))))
       (n1 (/ a (expt (- 1.0 (* e-squared (expt (sin fp) 2.0))) 0.5)))
       (D (/ reasting (* n1 k0)))
       (q1 (* n1 (/ (tan fp) r1)))
       (q2 (/ (expt d 2.0) 2.0))
       (q3 (/ (* (+ 5 (* 3.0 t1) (* 10.0 c1) (* -4 (expt c1 2.0)) (* -9.0 e-prime-squared)) (expt D 4.0)) 24.0))
       (q4 (/ (* (+ 61 (* 90.0 t1) (* 298.0 c1) (* 45.0 (expt t1 2.0)) (* -3.0 (expt c1 2.0)) (* -252 e-prime-squared)) (expt D 6.0)) 720.0))
       (q5 d)
       (q6 (/ (* (+ 1.0 (* 2.0 t1) c1) (expt d 3.0)) 6.0))
       (q7 (/ (* (+ 5.0 (* -2.0 c1) (* 28.0 t1) (* -3 (expt c1 2.0)) (* 8.0 e-prime-squared) (* 24.0 (expt t1 2.0))) (expt d 5.0)) 120.0))
       (lat (- fp (* q1 (+ q2 (- q3) q4))))
       (lon (+ long0 (/ (+ q5 (- q6) q7) (cos fp)))))
    (declare (type double-float reasting a b f e-squared e-prime-squared long0 M mu
                   e1 j1 j2 j3 j4 fp c1 t1 r1 n1 D q1 q2 q3 q4 q5 q6 q7 lat lon))
                  
    (list (rad2deg (realpart lat)) (rad2deg (realpart lon)))))

