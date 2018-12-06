;; -*- coding: utf-8 -*-
(defpackage :rotate-hue
  (:use :cl :opticl :dufy)
  (:export #:rotate-hue #:main))

(in-package :rotate-hue)

(define-cat-function d65-to-c +illum-d65+ +illum-c+)
(define-cat-function c-to-d65 +illum-c+ +illum-d65+)

(declaim (inline qrgb-to-mhvc))
(defun qrgb-to-mhvc (qr qg qb)
  "Converts quantized sRGB to Munsell HVC."
  (declare (inline qrgb-to-xyz xyz-to-mhvc-illum-c))
  (multiple-value-call #'xyz-to-mhvc-illum-c
   (multiple-value-call #'d65-to-c
     (qrgb-to-xyz qr qg qb))
    :if-reach-max :error))

(declaim (inline mhvc-to-qrgb))
(defun mhvc-to-qrgb (hue40 value chroma)
  "Converts Munsel HVC to (no clamped) quantized sRGB."
  (declare (inline xyz-to-qrgb mhvc-to-xyz-illum-c))
  (multiple-value-call #'xyz-to-qrgb
    (multiple-value-call #'c-to-d65
      (mhvc-to-xyz-illum-c hue40 value chroma))
    :clamp nil))

(defmacro nlet (name args &body body)
  `(labels ((,name ,(mapcar #'car args) ,@body))
     (,name ,@(mapcar #'cadr args))))

(declaim (inline mhvc-to-qrgb-within-gamut))
(defun mhvc-to-qrgb-within-gamut (hue40 value chroma)
  "Converts Munsell HVC to quantized sRGB. Preserves the given value
and hue even if the color is out of gamut."
  (declare (optimize (speed 3) (safety 1)))
  (macrolet ((within-gamut-p (q1 q2 q3)
               `(and (<= 0 ,q1 255) (<= 0 ,q2 255) (<= 0 ,q3 255)))
             (clamp (x) `(if (< ,x 0)
                             0
                             (if (< 255 ,x)
                                 255
                                 ,x))))
    (multiple-value-bind (qr qg qb) (mhvc-to-qrgb hue40 value chroma)
      (if (within-gamut-p qr qg qb)
          (values qr qg qb)
          ;; Finds the largest chroma that doesn't exceed the sRGB
          ;; space.
          (nlet %bisect ((chroma-lo 0d0) (chroma-hi chroma) (prev-qr qr) (prev-qg qg) (prev-qb qb))
            (declare (double-float chroma-lo chroma-hi)
                     (fixnum prev-qr prev-qg prev-qb))
            (when (< (random 1d0) 0.001d0))
            (let ((chroma-mid (* 0.5d0 (+ chroma-lo chroma-hi))))
              (multiple-value-bind (qr qg qb) (mhvc-to-qrgb hue40 value chroma-mid)
                (if (within-gamut-p qr qg qb)
                    (if (and (= qr prev-qr) (= qg prev-qg) (= qb prev-qb))
                        (values qr qg qb)
                        ;; handles the case qr, qg, or qb converge to
                        ;; e.g. -1 or 256.
                        (if (< (- chroma-hi chroma-lo) 1d-4)
                            (values (clamp qr) (clamp qg) (clamp qb))
                            (%bisect chroma-mid chroma-hi qr qg qb)))
                    (%bisect chroma-lo chroma-mid qr qg qb)))))))))

(defun rotate-hue (path delta-hue100 &optional (out *standard-output*))
  (declare (optimize (speed 3) (safety 1)))
  (let ((img (read-image-file path))
        (type (intern (string-upcase (pathname-type path)) :keyword))
        (delta-hue40 (* (float delta-hue100 1d0) 0.4d0)))
    (etypecase img
      (8-bit-rgb-image
       (locally (declare (type 8-bit-rgb-image img))
         (with-image-bounds (height width) img
           (dotimes (i height)
             (dotimes (j width)
               (multiple-value-bind (h v c)
                   (multiple-value-call #'qrgb-to-mhvc (pixel img i j))
                 (setf (pixel img i j)
                       (mhvc-to-qrgb-within-gamut (+ h delta-hue40) v c))))))))
      (8-bit-rgba-image
       (locally (declare (type 8-bit-rgba-image img))
         (with-image-bounds (height width) img
           (dotimes (i height)
             (dotimes (j width)
               (multiple-value-bind (qr qg qb qa) (pixel img i j)
                 (multiple-value-bind (h v c) (qrgb-to-mhvc qr qg qb)
                   (multiple-value-bind (res-qr res-qg res-qb)
                       (mhvc-to-qrgb-within-gamut (+ h delta-hue40) v c)
                     (setf (pixel img i j)
                           (values res-qr res-qg res-qb qa)))))))))))
    (write-image-stream out type img)))

(defun print-usage (&optional (out *error-output*))
  (format out "usage: rotate-hue INPUT-FILE ΔHUE [> OUTPUT-FILE]

ΔHUE: perimeter is 100"))

(defun main ()
  (handler-bind ((condition (lambda (c)
                              (uiop:println c *error-output*)
                              (uiop:quit 1))))
    (let ((args (uiop:command-line-arguments)))
      (if (/= (length args) 2)
          (print-usage)
          (rotate-hue (parse-namestring (first args))
                      (read-from-string (second args)))))))

