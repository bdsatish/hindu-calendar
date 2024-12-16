;;; hindu-calendar.el --- Arithmetical traditional Hindu calendar (panchanga)  -*- lexical-binding:t -*-

;; Copyright (C) 2024 B.D.Satish <bdsatish@gmail.com>

;; Author: B.D.Satish <bdsatish@gmail.com>
;; Maintainer: B.D.Satish <bdsatish@gmail.com>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: calendar, panchanga, Hindu, Indian
;; URL: https://github.com/bdsatish/hindu-calendar
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Created: 13-Dec-2024

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
;; details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides traditional Hindu calendars (solar and lunar) using
;; arithmetic based on the mean motions of the Sun and Moon.  It provides both
;; tropical (sayana) and sidereal (nirayana/Lahiri) variants.  It calculates
;; tithi and nakshatra.
;;
;; Usage:
;;     All of the functions can be called interactively or programmatically.
;;
;; Sidereal lunar (amanta): M-x hindu-calendar-sidereal-lunar
;; Tropical lunar (amanta): M-x hindu-calendar-tropical-lunar
;; Sidereal solar:          M-x hindu-calendar-sidereal-solar
;; Tropical solar:          M-x hindu-calendar-tropical-solar
;; Nakshatra (sidereal):    M-x hindu-calendar-asterism
;;
;; See README.md for more instructions and customizations.

;;; Installation:
;;
;; This package is found on MELPA, do `M-x package-install RET hindu-calendar`.
;; If you want to install it manually, clone this repository somewhere, add it
;; to `load-path`, and add `(require 'hindu-calendar)` to `.emacs`.

;;; Sanity:
;; M-x package-lint-current-buffer  (after M-x reinstall-package RET package-lint)
;; M-x checkdoc
;; M-x byte-compile-file

;;; References:
;; 1. Length of Hindu months taken from pg. 11 of
;;      https://www.packolkata.gov.in/INDIAN_CALADAR_PAC.pdf
;; 2. Solar and lunar constants from pg. 9 of Indian Astronomical Ephemeris:
;;      https://www.packolkata.gov.in/indian-astronomical-ephemeris.php

;;; Code:
;; Divided into two parts, backend and frontend.

(require 'calendar) ; only for (hindu-calendar--print-to-echo)

;;------------------------------ BACKEND CODE ----------------------------------

; floor(m) is exactly equivalent to Python's // operator.
; -14//3 = (floor -14 3) = -5
; int(-14/3) = (truncate -14 3) = -4
; (defun // (m &optional n) (floor m n))

;;; Helper functions:

; Linear search in a sorted array.
(defun hindu-calendar--linear-search-between (array x)
  "Return the position `i' in the sorted `ARRAY' such that a[i-1] <= `X' < a[i]."
  (let ((len (length array))
        (pos 0))
    (catch 'break
      (dotimes (i (1- len))
        (when (and (<= (nth i array) x)
                   (< x (nth (1+ i) array)))
          (setq pos (1+ i))
          (throw 'break pos))))
    pos))

;; Gregorian and vice versa
; 'absolute' => 'fixed' date, i.e. R.D. 'astro' implied JDN.
; NOTE: Have to write my own functions because built-in are buggy for negative years!
; ELISP> (calendar-gregorian-from-absolute
;        (calendar-absolute-from-gregorian (list 12 16 -2024)))
;  (1 -16 -2022) ; expected (12 16 -2024)
; works for all dates, even negative JDN. Algo from wikipedia
(defun hindu-calendar--gregorian-to-jdn (year month day)
  "Convert proleptic Gregorian `YEAR', `MONTH' and `DAY' to Julian Day Number."
  ; adjust for Jan and Feb as months 13 and 14 of previous year
  (let* ((year (if (<= month 2) (1- year) year))
         (month (if (<= month 2) (+ 12 month) month))
         (a (floor year 100))
         (b (+ (- 2 a) (floor a 4)))
         (c (floor (* 365.25 (+ year 4716))))
         (d (floor (* 30.6001 (1+ month))))
         (jdn (+ b c d day -1524.5)))
    jdn))

; works for all dates, even negative JDN. Algo from wikipedia
(defun hindu-calendar--gregorian-from-jdn (jdn)
  "Convert Julian Day Number `JDN' to proleptic Gregorian list (YEAR MONTH DAY)."
  (let* ((j (ceiling jdn))
         (k (* 3 (floor (+ (* 4 j) 274277) 146097)))
         (f (+ 1401 j -38 (floor k 4)))
         (e (+ 3 (* 4 f)))
         (g (floor (mod e 1461) 4))
         (h (+ 2 (* 5 g)))
         (day (1+ (floor (mod h 153) 5)))
         (month (1+ (mod (+ 2 (floor h 153)) 12)))
         (year (+ (floor e 1461) -4716 (floor (- 14 month) 12))))
    (list year month day)))

; Fixed Date = Rata Die, whose epoch is 01/Jan/1 CE proleptic Gregorian
(defun hindu-calendar--gregorian-to-fixed (y m d)
  "Convert proleptic Gregorian year `Y', month `M' and date `D' to fixed."
  (floor (+ -1721424.5 (hindu-calendar--gregorian-to-jdn y m d))))

(defun hindu-calendar--gregorian-from-fixed (fixed)
    "Convert `FIXED' date to proleptic Gregorian list (YEAR MONTH DAY)."
    (hindu-calendar--gregorian-from-jdn (- fixed -1721424.5)))

(defun hindu-calendar--fracday (hour &optional min sec)
  "`HOUR', `MIN'utes and `SEC'onds to fraction of a day.  Say 6.A.M.  = 0.25 day."
  (let ((min (or min 0.0))
        (sec (or sec 0.0)))
    (+ (/ hour 24.0) (/ min 1440.0) (/ sec 86400.0))))

; average length of a month from (sayana) Vaisakha to (sayana) Chaitra
; (setq mean-days '(0.0 30.4758333 30.9788888 31.3403472 31.4545833 31.2868750 30.8879861
;                   30.3737499 29.8851388 29.5477083 29.4434722 29.5961805 29.9714583)
; If you add all the above, it gives 365.2422 which is actually tropical year!
; Cumulative sum of above array is given below
(defconst hindu-calendar--tropical-transits
  '(0.0 30.4758333 61.4547221 92.7950693 124.2496526 155.5365276
    186.4245137 216.7982636 246.6834024 276.2311107 305.6745829
    335.2707634 365.2422217))

; Sidereal year is longer than tropical, so equally distribute the difference to all months
; (mapcar (lambda (elem) (+ elem (/ (- 365.256363 365.2422) 12))) mean-days)
; Cumulative sum of above array is given below
(defconst hindu-calendar--sidereal-transits
  '(0.0 30.47701188 61.45707927 92.79860505 124.25436693
    155.54242052 186.43158520 216.80651368 246.69283107
    276.24171795 305.68636873 335.28372782 365.25636470))

(defun hindu-calendar--solar-calendar-from-fixed (fixed tropicalp)
  "Return solar date, tropical if `TROPICALP' else sidereal, given `FIXED' date."
  ; Tropical solar epoch is sayana Sun in 0° Ar 22/Mar/-3101 = -1132901 R.D.
  ; Sidereal (Lahiri) solar epoch is Sun in 0° Ar = 01/Feb/-3101
  (let* ((epoch (if tropicalp -1132901 -1132949))
         (transits (if tropicalp
		       hindu-calendar--tropical-transits
		       hindu-calendar--sidereal-transits))
         (len-year (car (last transits))) ; last elem is length of year
        (sun ; sunrise on that date
          (+ (- fixed epoch) (hindu-calendar--fracday 6)))
         (year ; elapsed years
          (floor sun len-year))
         (ordinal ; ordinal day number in this year after `year's are elapsed
          (- sun (* year len-year)))
         (month (hindu-calendar--linear-search-between transits ordinal))
         (prev-month (nth (1- month) transits))
         (day (ceiling (- ordinal prev-month)))) ; leftover days after previous month
    (list year month day)))

; wrappers for above
(defun hindu-calendar--sidereal-solar-from-fixed (fixed)
  "Return sidereal solar date (YEAR MONTH DAY), given `FIXED' date."
  (hindu-calendar--solar-calendar-from-fixed fixed nil))

(defun hindu-calendar--tropical-solar-from-fixed (fixed)
    "Return tropical solar date (YEAR MONTH DAY), given `FIXED' date."
    (hindu-calendar--solar-calendar-from-fixed fixed t))

(defun hindu-calendar--lunar-calendar-from-fixed (fixed tropicalp)
  "Return lunar date, tropical if `TROPICALP' else sidereal, given `FIXED' date."
  ; Sayana Caitra-S1 in Ujjain falls on Feb 20/21, -3101 19:00 proleptic Gregorian = R.D -1132930 + 19/24.
  ; Nirayana Caitra-S1 in Ujjain falls on Jan 22, -3101 proleptic Gregorian = R.D -1132960 + 12/24.
  (let* ((epoch (if tropicalp -1132930 -1132959))
         (len-year (if tropicalp 365.242190 365.256363))
         (synodic-month 29.5305889) ; days b/w successive new-moons (or full-moons)
         (lunar-day (/ synodic-month 30)) ; 30 tithis in a lunar month
         (sun ; sunrise on that date
          (+ (- fixed epoch) (hindu-calendar--fracday 6)))
         (new-moon ; start of lunar month
          (- sun (mod sun synodic-month)))
         (leap-mon (hindu-calendar--leap-month-p
		    (+ new-moon epoch) tropicalp synodic-month))
         (month (1+ (% (nth 0 leap-mon) 12))) ; next solar month
         (leap? (nth 1 leap-mon))
         (day ; tithis since beginning of lunar month
          (1+ (mod (floor sun lunar-day) 30)))
         (year ; solar year at end of lunar month(s)
          (floor new-moon len-year))) ; new year begins on epoch
    (if (string= "purnimanta" (downcase hindu-calendar-lunar-type))
        (hindu-calendar--convert-to-purnimanta year month leap? day)
      (list year month leap? day)))) ; default is amanta

; synodic-month = difference b/w consecutive new moons (by definition)
(defun hindu-calendar--leap-month-p (new-moon tropicalp synodic-month)
  "Check if fixed date `NEW-MOON' falls within same solar month.
`TROPICALP' = t or nil.  `SYNODIC-MONTH' = 29.5305889 days."
  ; if new-moon (or sukla-pratipada?) of this month & next month fall
  ; within same solar month, then it is leap
  ; returns (solar-month-number, is-leap?)
  ; use (// new-moon) because (solar-calendar-from-fixed) expects fixed date
  ; and will add 6 A.M. automatically
  (let* ((offset (if tropicalp 0 -0.5))  ; why? S1 for sidereal, amavasya for tropical
         (now (hindu-calendar--solar-calendar-from-fixed
	       (+ offset (floor new-moon)) tropicalp))
         (next (hindu-calendar--solar-calendar-from-fixed
		(+ offset (floor (+ new-moon synodic-month))) tropicalp)))
    (list (nth 1 now) (= (nth 1 now) (nth 1 next))))) ; M in (Y M D) is second element of array

(defun hindu-calendar--convert-to-purnimanta (year month leap? day)
  "Convert given amanta date (`YEAR' `MONTH' `LEAP?' `DAY') to purnimanta."
  (if (and (> day 15) (not leap?)) ; bump month only for krishna paksha
      (list year (1+ (mod month 12)) leap? day)
    (list year month leap? day)))

; wrappers for above
(defun hindu-calendar--sidereal-lunar-from-fixed (fixed)
    "Return sidereal lunar date (YEAR MONTH LEAP-MONTH? DAY), given `FIXED' date."
    (hindu-calendar--lunar-calendar-from-fixed fixed nil))

(defun hindu-calendar--tropical-lunar-from-fixed (fixed)
    "Return tropical lunar date (YEAR MONTH LEAP-MONTH? DAY), given `FIXED' date."
    (hindu-calendar--lunar-calendar-from-fixed fixed t))

; Daily nakshatra as per Lahiri ayanamsha
(defun hindu-calendar--nakshatra (fixed)
  "Return the lunar mansion (nakshatra) on `FIXED' date.  1= Asvini,.., 27= Revati."
  ; Sidereal (Lahiri) solar epoch is when Sun in 0° Ar = 01/Feb/-3101 = -1132949 R.D, Aslesha (#9)
  (let* ((epoch -1132950.375) ; 15:00 on 31/Jan/-3101 is when Ashlesha begins at Ujjain
	 (nakshatra-month 27.3216615625) ; num. days in sidereal month (fixed star to fixed star)
	 (nakshatra-day (/ nakshatra-month 27)) ; 27 nak. in a month
	 (nak0 8) ; ashlesha on epoch, nak. counting from 0
	 (sun ; sunrise on that date
	  (+ (- fixed epoch) (hindu-calendar--fracday 6))))
    (1+ (mod (+ nak0 (floor sun nakshatra-day)) 27)))) ; nak. since epoch

;;------------------------------ FRONTEND CODE ----------------------------------

(defgroup hindu-calendar-group nil
  "Settings for Hindu calendar."
  :group 'calendar)

(defcustom hindu-calendar-month-type "Chaitra"
  "Type of month names.  One of Chaitra, Mesha, Madhu, Kesava, or Dhata."
  :type '(choice (const :tag "Chaitra, Vaisakha,..." "Chaitra")
                 (const :tag "Mesha, Vrishabha,..." "Mesha")
                 (const :tag "Madhu, Madhava,..." "Madhu")
                 (const :tag "Dhata, Aryaman,..." "Dhata")
                 (const :tag "Baisakha, Jyestha,..." "Baisakha")
		 (const :tag "Kesava, Narayana,..." "Kesava"))
  :group 'hindu-calendar-group)

(defcustom hindu-calendar-epoch-type "Kali"
  "Type of epoch to reckon years.  One of Kali, Vikrama, Saka, or Bengali."
  :type '(choice (const :tag "Kali Yuga (elapsed)" "Kali")
                 (const :tag "Vikrama samvat" "Vikrama")
                 (const :tag "Salivahana saka" "Saka")
                 (const :tag "Bengali san" "Bengali"))
  :group 'hindu-calendar-group)

(defcustom hindu-calendar-lunar-type "Amanta"
  "Type of lunar calendar: purnimanta for full-moon, amanta for new-moon."
  :type '(choice (const :tag "Month ends on a new-moon day" "Amanta")
                 (const :tag "Month ends on a full-moon day" "Purnimanta"))
  :group 'hindu-calendar-group)

; Spelling as per the Rashtriya Panchang
(defconst hindu-calendar--chaitra-months
  (list "" "Chaitra" "Vaisakha" "Jyaishtha" "Ashadha" "Sravana" "Bhadrapada"
        "Asvina" "Kartika" "Margasirsa" "Pausha" "Magha" "Phalguna"))

(defconst hindu-calendar--mesha-months
  (list "" "Mesha" "Vrishabha" "Mithuna" "Karkata" "Simha" "Kanya"
	"Tula" "Vrischika" "Dhanus" "Makara" "Kumbha" "Mina"))

(defconst hindu-calendar--madhu-months
  (list "" "Madhu" "Madhava" "Sukra" "Suchi" "Nabhas" "Nabhasya"
	"Isha" "Urja" "Sahas" "Sahasya" "Tapas" "Tapasya"))

(defconst hindu-calendar--kesava-months
  (list "" "Vishnu" "Madhusudana" "Trivikrama" "Vamana" "Sridhara" "Hrishikesa"
	"Padmanabha" "Damodara" "Kesava" "Narayana" "Madhava" "Govinda"))

(defconst hindu-calendar--dhata-months
  (list "" "Dhata" "Aryama" "Mitra" "Varuna" "Indra" "Vivasvan"
	"Tvashta" "Vishnu" "Amsuman" "Bhaga" "Pusha" "Parjanya"))

(defconst hindu-calendar--baisakha-months
  (list "" "Baisakha" "Jyestha" "Asadha" "Srabana" "Bhadra" "Asvina"
	"Kartika" "Margasira" "Pousha" "Magha" "Phalguna" "Chaitra"))

(defconst hindu-calendar--nakshatra-names
  (list "" "Asvini" "Bharani" "Krittika" "Rohini" "Mrigasiras" "Ardra"
        "Punarvasu" "Pushya" "Aslesha" "Magha" "Purvaphalguni"
	"Uttaraphalguni" "Hasta" "Chitra" "Svati" "Visakha" "Anuradha"
	"Jyeshtha" "Mula" "Purvashadha" "Uttarashadha" "Sravana"
	"Dhanishta" "Satabhishaj" "Purvabhadra" "Uttarabhadra" "Revati"))

(defun hindu-calendar--tithi-to-paksha (tithi)
  "Convert given `TITHI' into krishna-paksha (K) or shukla-paksha (S)."
  (if (> tithi 15)
      (format "K%02d" (- tithi 15))
      (format "S%02d" tithi)))

;; Conversion functions for epochs. `checkdoc' does not complain about `fset'.
; (fset 'hindu-calendar--vikrama (lambda (kali) (- kali 3044)))

(defun hindu-calendar--convert-epoch (year)
  "Convert Kali-yuga elapsed `YEAR' into epoch type (Saka, Vikrama,...)."
  (cond
   ((string= "vikrama" (downcase hindu-calendar-epoch-type))
    (- year 3044))
   ((string=  "saka" (downcase hindu-calendar-epoch-type))
    (- year 3179))
   ((string= "bengali" (downcase hindu-calendar-epoch-type))
    (- year 3694))
   (t year))) ; default is Kali Year itself

(defun hindu-calendar--convert-month (month leap-month-p)
  "Convert `MONTH' number to string, accounting for `LEAP-MONTH-P'."
  (cond
   ((string= (downcase hindu-calendar-month-type) "mesha")
    (nth month hindu-calendar--mesha-months))
   ((string= (downcase hindu-calendar-month-type) "madhu")
    (if leap-month-p "Amhaspati" (nth month hindu-calendar--madhu-months)))
   ((string= (downcase hindu-calendar-month-type) "kesava")
    (if leap-month-p "Purushottama" (nth month hindu-calendar--kesava-months)))
   ((string= (downcase hindu-calendar-month-type) "dhata")
    (nth month hindu-calendar--dhata-months))
   ((string= (downcase hindu-calendar-month-type) "baisakha")
    (nth month hindu-calendar--baisakha-months))
   (t
    (if leap-month-p
	(concat "Adhika-" (nth month hindu-calendar--chaitra-months))
	(nth month hindu-calendar--chaitra-months))))) ; default is Chaitra-type

(defun hindu-calendar--print-to-echo ()
  "Print Hindu sidereal dates to echo area."
  (interactive)
  (let* ((date (calendar-cursor-to-date))
         (day (nth 1 date))
         (month (nth 0 date))
         (year (nth 2 date)))
    (message "Hindu date: Lunar %s nakshatra %s; Solar %s"
	    (hindu-calendar-asterism year month day)
	    (hindu-calendar-sidereal-lunar year month day)
	    (hindu-calendar-sidereal-solar year month day))))

(define-key calendar-mode-map (kbd "p H") 'hindu-calendar--print-to-echo)

;;;###autoload
(defun hindu-calendar-tropical-solar (&optional year month date)
  "Return Hindu tropical solar date of proleptic Gregorian `YEAR' `MONTH' `DATE'.
It is equivalent to Indian National Calendar civil date used by the Indian govt."
  (interactive)
  (let* ((now (decode-time)); returns (ss mm hh day month year ...)
         (year (or year (nth 5 now))) ; use (now) if val is not set
         (month (or month (nth 4 now))) ; use (now) if val is not set
         (date (or date (nth 3 now))) ; use (now) if val is not set
         (rdie (hindu-calendar--gregorian-to-fixed year month date))
         (h-date (hindu-calendar--tropical-solar-from-fixed rdie))
         (h-year (nth 0 h-date)) ; unpack results
         (h-month (nth 1 h-date))
         (h-day (nth 2 h-date))
	 (result ""))
    (setq result (format "%s-%02d, %d"
                         (hindu-calendar--convert-month h-month nil)
                         h-day
                         (hindu-calendar--convert-epoch h-year)))
    (if (called-interactively-p 'any) (insert result) result)))

;;;###autoload
(defun hindu-calendar-tropical-lunar (&optional year month date)
  "Return Hindu tropical lunar date of proleptic Gregorian `YEAR' `MONTH' `DATE'."
  (interactive)
  (let* ((now (decode-time)); returns (ss mm hh day month year ...)
         (year (or year (nth 5 now))) ; use (now) if val is not set
         (month (or month (nth 4 now))) ; use (now) if val is not set
         (date (or date (nth 3 now))) ; use (now) if val is not set
         (rdie (hindu-calendar--gregorian-to-fixed year month date))
         (hindu-date (hindu-calendar--tropical-lunar-from-fixed rdie))
         (h-year (nth 0 hindu-date)) ; unpack results
         (h-month (nth 1 hindu-date))
         (h-leap? (nth 2 hindu-date))
         (h-day (nth 3 hindu-date))
	 (result ""))
    (setq result (format "%s-%s, %d"
                         (hindu-calendar--convert-month h-month h-leap?)
                         (hindu-calendar--tithi-to-paksha h-day)
                         (hindu-calendar--convert-epoch h-year)))
    (if (called-interactively-p 'any) (insert result) result)))

;;;###autoload
(defun hindu-calendar-sidereal-solar (&optional year month date)
  "Return sidereal/Lahiri solar date of proleptic Gregorian `YEAR' `MONTH' `DATE'."
  (interactive)
  (let* ((now (decode-time)); returns (ss mm hh day month year ...)
         (year (or year (nth 5 now))) ; use (now) if val is not set
         (month (or month (nth 4 now))) ; use (now) if val is not set
         (date (or date (nth 3 now))) ; use (now) if val is not set
         (rdie (hindu-calendar--gregorian-to-fixed year month date))
         (h-date (hindu-calendar--sidereal-solar-from-fixed rdie))
         (h-year (nth 0 h-date)) ; unpack results
         (h-month (nth 1 h-date))
         (h-day (nth 2 h-date))
	 (result ""))
    (setq result (format "%s-%02d, %d"
                         (hindu-calendar--convert-month h-month nil)
                         h-day
                         (hindu-calendar--convert-epoch h-year)))
    (if (called-interactively-p 'any) (insert result) result)))

;;;###autoload
(defun hindu-calendar-sidereal-lunar (&optional year month date)
  "Return Hindu sidereal lunar date of proleptic Gregorian `YEAR' `MONTH' `DATE'."
  (interactive)
  (let* ((now (decode-time)); returns (ss mm hh day month year ...)
         (year (or year (nth 5 now))) ; use (now) if val is not set
         (month (or month (nth 4 now))) ; use (now) if val is not set
         (date (or date (nth 3 now))) ; use (now) if val is not set
         (rdie (hindu-calendar--gregorian-to-fixed year month date))
         (hindu-date (hindu-calendar--sidereal-lunar-from-fixed rdie))
         (h-year (nth 0 hindu-date)) ; unpack results
         (h-month (nth 1 hindu-date))
         (h-leap? (nth 2 hindu-date))
         (h-day (nth 3 hindu-date))
	 (result ""))
    (setq result (format "%s-%s, %d"
                         (hindu-calendar--convert-month h-month h-leap?)
                         (hindu-calendar--tithi-to-paksha h-day)
                         (hindu-calendar--convert-epoch h-year)))
    (if (called-interactively-p 'any) (insert result) result)))

;;;###autoload
(defun hindu-calendar-asterism (&optional year month date)
  "Return sidereal lunar nakshatra on proleptic Gregorian `YEAR' `MONTH' `DATE'."
  (interactive)
  (let* ((now (decode-time)); returns (ss mm hh day month year ...)
         (year (or year (nth 5 now))) ; use (now) if val is not set
         (month (or month (nth 4 now))) ; use (now) if val is not set
         (date (or date (nth 3 now))) ; use (now) if val is not set
         (rdie (hindu-calendar--gregorian-to-fixed year month date))
         (nakshatra (hindu-calendar--nakshatra rdie))
	 (result (nth nakshatra hindu-calendar--nakshatra-names)))
    (if (called-interactively-p 'any) (insert result) result)))

;;----------------------------- PROVIDE PACKAGE ---------------------------------
(provide 'hindu-calendar)
;;; hindu-calendar.el ends here
