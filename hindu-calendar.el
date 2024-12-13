;;; hindu-calendar.el --- Arithmetic Hindu calendar (panchanga) for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2025 B.D.Satish <bdsatish@gmail.com>

;; Author: B.D.Satish <bdsatish@gmail.com>
;; Maintainer: B.D.Satish <bdsatish@gmail.com>
;; Version: 0.1
;; Created: 13-Dec-2024
;; Keywords: calendar, panchanga, Hindu, Indian
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
;; details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides traditional Hindu calendars (solar and lunar) using
;; arithmetic based on the mean motions of Sun and Moon. It provides both
;; tropical (sayana) and sidereal (nirayana/Lahiri) variants. It calculates
;; tithi, nakshatra and vaara.

;;; Installation

;; This package is available on MELPA, just `M-x` `package-install`
;; `hindu-calendar`. If you want to install it manually, clone this repository
;; somewhere, add it to `load-path`, and add `(require 'hindu-calendar)` to
;; `.emacs`.

;;; Configuration

;; Sidereal lunar (amanta): M-x hindu-cal-insert-sidereal-lunar
;; Tropical lunar (amanta): M-x hindu-cal-insert-tropical-lunar
;; Sidereal solar:          M-x hindu-cal-insert-sidereal-solar
;; Tropical solar:          M-x hindu-cal-insert-tropical-solar


;;; Helper functions
(defun first  (array) (nth 0 array))
(defun second (array) (nth 1 array))
(defun third  (array) (nth 2 array))

; floor(m) is exactly equivalent to Python's // operator.
; -14//3 = (floor -14 3) = -5
; int(-14/3) = (truncate -14 3) = -4
(defun // (m &optional n) (floor m n))

; Linear search in a sorted array
(defun linear-search-between (array x)
  "Returns the position `i' in the sorted `array' such that a[i-1] <= x < a[i]"
  (let ((len (length array))
        (pos 0))
    (catch 'break
      (dotimes (i (1- len))
        (when (and (<= (nth i array) x)
                   (< x (nth (1+ i) array)))
          (setq pos (1+ i))
          (throw 'break pos))
      )
    )
  pos)
)

;; Gregorian and vice versa
; works for all dates, even negative JDN. Algo from wikipedia
(defun gregorian-to-jdn (year month day)
  ; adjust for Jan and Feb as months 13 and 14 of previous year
  (let* ((year (if (<= month 2) (1- year) year))
         (month (if (<= month 2) (+ 12 month) month))
         (a (// year 100))
         (b (+ (- 2 a) (// a 4)))
         (c (// (* 365.25 (+ year 4716)))) ; same as floor(...)
         (d (// (* 30.6001 (1+ month))))   ; same as floor(...)
         (jdn (+ b c d day -1524.5))
        )
    jdn
  )
)

; works for all dates, even negative JDN. Algo from wikipedia
(defun gregorian-from-jdn (jdn)
  (let* ((j (ceiling jdn))
         (k (* 3 (// (+ (* 4 j) 274277) 146097)))
         (f (+ 1401 j -38 (// k 4)))
         (e (+ 3 (* 4 f)))
         (g (// (mod e 1461) 4))
         (h (+ 2 (* 5 g)))
         (day (1+ (// (mod h 153) 5)))
         (month (1+ (mod (+ 2 (// h 153)) 12)))
         (year (+ (// e 1461) -4716 (// (- 14 month) 12)))
        )
    (list year month day))
)

; Fixed Date = Rata Die, whose epoch is 01/Jan/1 CE proleptic Gregorian
(defun gregorian-to-fixed (y m d) (// (+ -1721424.5 (gregorian-to-jdn y m d))))
(defun gregorian-from-fixed (fixed) (gregorian-from-jdn (- fixed -1721424.5)))

;; hours, minutes and seconds to fraction of a day. E.g. 6.A.M. = 0.25 day
(defun fracday (hour &optional min sec)
  (let ((min (or min 0.0))
        (sec (or sec 0.0)))
    (+ (/ hour 24.0) (/ min 1440.0) (/ sec 86400.0))))

; Taken from pg. 11 of INDIAN_CALADAR_PAC.pdf
; average length of a month from (sayana) Vaisakha to (sayana) Chaitra
; (setq mean-days '(0.0 30.4758333 30.9788888 31.3403472 31.4545833 31.2868750 30.8879861
;                   30.3737499 29.8851388 29.5477083 29.4434722 29.5961805 29.9714583)
; If you add all the above, it gives 365.2422 which is actually tropical year!
; Cumulative sum of above array is given below
(defconst tropical-transits
  '(0.0 30.4758333 61.4547221 92.7950693 124.2496526 155.5365276
    186.4245137 216.7982636 246.6834024 276.2311107 305.6745829
    335.2707634 365.2422217))

; Sidereal year is longer than tropical, so equally distribute the difference to all months
; (mapcar (lambda (elem) (+ elem (/ (- 365.256363 365.2422) 12))) mean-days)
; Cumulative sum of above array is given below
(defconst sidereal-transits
  '(0.0 30.47701188 61.45707927 92.79860505 124.25436693
    155.54242052 186.43158520 216.80651368 246.69283107
    276.24171795 305.68636873 335.28372782 365.25636470))

(defun solar-calendar-from-fixed (fixed tropical?)
  "Returns solar date (sauramana) of given R.D. date"
  ; Tropical solar epoch is sayana Sun in 0° Ar 22/Mar/-3101 = -1132901 R.D.
  ; Sidereal (Lahiri) solar epoch is Sun in 0° Ar = 01/Feb/-3101
  (let* ((epoch (if tropical? -1132901 -1132949))
         (transits (if tropical? tropical-transits sidereal-transits))
         (len-year (car (last transits))) ; last elem is length of year
        (sun ; sunrise on that date
          (+ (- fixed epoch) (fracday 6)))
         (year ; elapsed years
          (// sun len-year))
         (ordinal ; ordinal day number in this year after `year's are elapsed
          (- sun (* year len-year)))
         (month (linear-search-between transits ordinal))
         (prev-month (nth (1- month) transits))
         (day (ceiling (- ordinal prev-month))) ; leftover days after previous month
       )
    (list year month day))
)

; wrappers for above
(defun sidereal-solar-from-fixed (fixed) (solar-calendar-from-fixed fixed nil))
(defun tropical-solar-from-fixed (fixed) (solar-calendar-from-fixed fixed t))

(defun lunar-calendar-from-fixed (fixed tropical?)
  ; Sayana Caitra-S1 in Ujjain falls on Feb 20/21, -3101 19:00 proleptic Gregorian = R.D -1132930 + 19/24.
  ; Nirayana Caitra-S1 in Ujjain falls on Jan 22, -3101 proleptic Gregorian = R.D -1132960 + 12/24.
  (let* ((epoch (if tropical? -1132930 -1132959))
         (len-year (if tropical? 365.242190 365.256363))
         (synodic-month 29.5305889) ; days b/w successive new-moons (or full-moons)
         (lunar-day (/ synodic-month 30)) ; 30 tithis in a lunar month
         (sun ; sunrise on that date
          (+ (- fixed epoch) (fracday 6)))
         (new-moon ; start of lunar month
          (- sun (mod sun synodic-month)))
         (leap-mon (leap-month? (+ new-moon epoch) tropical? synodic-month))
         (month (1+ (% (first leap-mon) 12))) ; next solar month
         (leap (second leap-mon))
         (day ; tithis since beginning of lunar month
          (1+ (mod (// sun lunar-day) 30)))
         (year ; solar year at end of lunar month(s)
          (// new-moon len-year))) ; new year begins on epoch
    (list year month leap day)
  )
)

; synodic-month = difference b/w consecutive new moons (by definition)
(defun leap-month? (new-moon tropical? synodic-month)
  ; if new-moon (or sukla-pratipada?) of this month & next month fall
  ; within same solar month, then it is leap
  ; returns (solar-month-number, is-leap?)
  ; use (// new-moon) because (solar-calendar-from-fixed) expects fixed date
  ; and will add 6 A.M. automatically
  (let* ((offset (if tropical? 0 -0.5))  ; why? S1 for sidereal, amavasya for tropical
         (s1-now (solar-calendar-from-fixed (+ offset (// new-moon)) tropical?))
         (s1-next (solar-calendar-from-fixed (+ offset (// (+ new-moon synodic-month))) tropical?))
        )
    (print new-moon)
    (print s1-now)
    (print s1-next)
    (list (second s1-now) (= (second s1-now) (second s1-next)))
  )
)

; wrappers for above
(defun sidereal-lunar-from-fixed (fixed) (lunar-calendar-from-fixed fixed nil))
(defun tropical-lunar-from-fixed (fixed) (lunar-calendar-from-fixed fixed t))

; Daily nakshatra as per Lahiri ayanamsha
(defun nakshatra (fixed)
  "Returns the lunar mansion (nakshatra) at the time of sunrise. 1 = Asvini,...,27 = Revati"
  ; Sidereal (Lahiri) solar epoch is Sun in 0° Ar = 01/Feb/-3101 = -1132949 R.D, Aslesha (#9)
  (let* ((epoch -1132950.375) ; 15:00 on 31/Jan/-3101 is when Ashlesha begins at Ujjain
	 (nakshatra-month 27.3216615625) ; num. days in sidereal month (fixed star to fixed star)
	 (nakshatra-day (/ nakshatra-month 27)) ; 27 nak. in a month
	 (nak0 8) ; ashlesha on epoch, nak. counting from 0
	 (sun ; sunrise on that date
	  (+ (- fixed epoch) (fracday 6)))
	 )
    (1+ (mod (+ nak0 (// sun nakshatra-day)) 27)) ; nak. since epoch
  )
)

(provide 'hindu-calendar)

;;; hindu-calendar.el ends here
