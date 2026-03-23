;;; hindu-calendar.el --- Arithmetical traditional Hindu calendar (panchanga)  -*- lexical-binding:t -*-

;; Copyright (C) 2024-2026 B.D.Satish <bdsatish@gmail.com>

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
;; arithmetic based on the mean motions of the Sun and Moon.  It calculates
;; tithi and nakshatra.  It provides both tropical (sayana) and sidereal
;; (nirayana/Chitrapaksha) variants.  Lunar calendar can be chosen between
;; amanta or purnimanta.
;;
;; Usage:
;;     All of the functions can be called interactively or programmatically.
;;
;; (require 'hindu-calendar)
;;
;; Sidereal lunar:        M-x hindu-calendar-sidereal-lunar
;; Tropical lunar:        M-x hindu-calendar-tropical-lunar
;; Sidereal solar:        M-x hindu-calendar-sidereal-solar
;; Tropical solar:        M-x hindu-calendar-tropical-solar
;; Nakshatra (sidereal):  M-x hindu-calendar-asterism
;; Tweak variables:       M-x customize-group RET hindu-calendar
;;
;; (hindu-calendar-keybindings) ; Optional

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

(require 'calendar)
(require 'solar)
(require 'lunar)
(require 'cal-julian)

;;------------------------------ FRONTEND CODE ----------------------------------

(defgroup hindu-calendar nil
  "Options for Hindu calendar."
  :group 'calendar)

(defcustom hindu-calendar-month-type "Chaitra"
  "Type of month names.  One of Chaitra, Mesha, Madhu, Kesava, or Dhata."
  :type '(choice (const :tag "Chaitra, Vaisakha,..." "Chaitra")
                 (const :tag "Mesha, Vrishabha,..." "Mesha")
                 (const :tag "Madhu, Madhava,..." "Madhu")
                 (const :tag "Dhata, Aryaman,..." "Dhata")
                 (const :tag "Baisakha, Jyestha,..." "Baisakha")
		 (const :tag "Kesava, Narayana,..." "Kesava"))
  :group 'hindu-calendar)

(defcustom hindu-calendar-epoch-type "Kali"
  "Type of epoch to reckon years.  One of Kali, Vikrama, Saka, or Bengali."
  :type '(choice (const :tag "Kali Yuga (elapsed)" "Kali")
                 (const :tag "Vikrama samvat" "Vikrama")
                 (const :tag "Salivahana saka" "Saka")
                 (const :tag "Bengali san" "Bengali")
                 (const :tag "Saptarshi era (elapsed)" "Saptarshi"))
  :group 'hindu-calendar)

(defcustom hindu-calendar-lunar-type "Amanta"
  "Type of lunar calendar: purnimanta for full-moon, amanta for new-moon."
  :type '(choice (const :tag "Amanta, month ends on a new-moon day" "Amanta")
                 (const :tag "Purnimanta, month ends on a full-moon day" "Purnimanta"))
  :group 'hindu-calendar-group)

; Spelling as per the Rashtriya Panchang
(defconst hindu-calendar--chaitra-months
  (list "" "Chaitra" "Vaisakha" "Jyaishtha" "Ashadha" "Sravana" "Bhadrapada"
        "Asvina" "Kartika" "Margasirsa" "Pausha" "Magha" "Phalguna" "Chaitra"))

(defconst hindu-calendar--mesha-months
  (list "" "Mesha" "Vrishabha" "Mithuna" "Karkata" "Simha" "Kanya"
	"Tula" "Vrischika" "Dhanus" "Makara" "Kumbha" "Mina"))

(defconst hindu-calendar--madhu-months
  (list "" "Madhu" "Madhava" "Sukra" "Suchi" "Nabhas" "Nabhasya"
	"Isha" "Urja" "Sahas" "Sahasya" "Tapas" "Tapasya" "Madhu"))

(defconst hindu-calendar--kesava-months
  (list "" "Vishnu" "Madhusudana" "Trivikrama" "Vamana" "Sridhara" "Hrishikesa"
	"Padmanabha" "Damodara" "Kesava" "Narayana" "Madhava" "Govinda" "Vishnu"))

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

; See 'Book of Indian eras' by Alexander Cunningham and 'Indian Eras' by Kota Venkatachalam
(defun hindu-calendar--convert-epoch (year)
  "Convert Kali-yuga elapsed `YEAR' into epoch type (Saka, Vikrama,...)."
  (cond
   ((string= "saptarshi" (downcase hindu-calendar-epoch-type))
    (- year 26)) ; laukika era starts 3076 BCE (-3075)
   ((string= "vikrama" (downcase hindu-calendar-epoch-type))
    (- year 3044)) ; elapsed epoch 58 BCE (-57)
   ((string= "ce" (downcase hindu-calendar-epoch-type))
    (- year 3101)) ; Kali to CE/BCE Gregorian of today
   ((string=  "saka" (downcase hindu-calendar-epoch-type))
    (- year 3179)) ; Epoch 78 CE
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

(defun hindu-calendar-keybindings ()
  "Setup some useful keybindings."
  (define-key calendar-mode-map (kbd "p H") 'hindu-calendar--print-to-echo))

;;;###autoload
(defun hindu-calendar-tropical-solar (&optional year month date)
  "Return Hindu tropical solar date of proleptic Gregorian `YEAR' `MONTH' `DATE'.
It is equivalent to Indian National Calendar civil date used by the Indian govt."
  (interactive)
  (let* ((now (decode-time)); returns (ss mm hh day month year ...)
         (year (or year (nth 5 now))) ; use (now) if val is not set
         (month (or month (nth 4 now))) ; use (now) if val is not set
         (date (or date (nth 3 now))) ; use (now) if val is not set
         (h-date (hindu-calendar--tropical-solar-from-gregorian year month date))
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
         (hindu-date (hindu-calendar--tropical-lunar-from-gregorian year month date))
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
  "Return Hindu sidereal solar date of proleptic Gregorian `YEAR' `MONTH' `DATE'."
  (interactive)
  (let* ((now (decode-time)); returns (ss mm hh day month year ...)
         (year (or year (nth 5 now))) ; use (now) if val is not set
         (month (or month (nth 4 now))) ; use (now) if val is not set
         (date (or date (nth 3 now))) ; use (now) if val is not set
         (h-date (hindu-calendar--sidereal-solar-from-gregorian year month date))
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
         (hindu-date (hindu-calendar--sidereal-lunar-from-gregorian year month date))
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
         (nakshatra (hindu-calendar--nakshatra-from-gregorian year month date))
	 (result (nth nakshatra hindu-calendar--nakshatra-names)))
    (if (called-interactively-p 'any) (insert result) result)))

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
;                   30.3737499 29.8851388 29.5477083 29.4434722 29.5961805 29.9714583))
; If you add all the above, it gives 365.2422 which is actually tropical year!
; Cumulative sum of above array is given below
(defconst hindu-calendar--tropical-transits
  '(0.0 30.4758333 61.4547221 92.7950693 124.2496526 155.5365276
    186.4245137 216.7982636 246.6834024 276.2311107 305.6745829
    335.2707634 365.2422217))

; Sidereal year is longer than tropical, so equally distribute the difference to all months
; (mapcar (lambda (elem) (+ elem (/ (- 365.256363 365.24219) 12))) mean-days)
; Cumulative sum of above array is given below
(defconst hindu-calendar--sidereal-transits
  '(0.0 30.47701188 61.45707927 92.79860505 124.25436693
    155.54242052 186.43158520 216.80651368 246.69283107
    276.24171795 305.68636873 335.28372782 365.25636470))

; Garga's unequal spacing of nakshatras. Ending longitudes
(defconst hindu-calendar--garga-spacing
  '(0.0 13.33333334 20.0 33.33333334 53.33333334 66.66666667 73.33333334 93.33333334
    106.66666667 113.33333334 126.66666667 140.0 160.0 173.33333334 186.66666667
    193.33333334 213.33333334 226.66666667 233.33333334 246.66666667 260.0 280.0
    293.33333334 306.66666667 313.33333334 326.66666667 346.66666667 360.0))

(defun hindu-calendar--solar-calendar-from-fixed (fixed tropicalp)
  "Return solar date, tropical if `TROPICALP' else sidereal, given `FIXED' date."
  ; Tropical solar epoch is sayana Sun in 0° Ar 22/Mar/-3101 = -1132901 R.D.
  ; Sidereal (Chitrapaksha) solar epoch is Sun in 0° Ar = 01/Feb/-3101
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

; Daily nakshatra as per Chitrapaksha ayanamsha
(defun hindu-calendar--nakshatra-from-fixed (fixed)
  "Return the lunar mansion (nakshatra) on `FIXED' date.  1= Asvini,.., 27= Revati."
  ; Sidereal (Chitrapaksha) solar epoch is when Sun in 0° Ar = 01/Feb/-3101 = -1132949 R.D, Aslesha (#9)
  (let* ((epoch -1132950.375) ; 15:00 on 31/Jan/-3101 is when Ashlesha begins at Ujjain
	 (nakshatra-month 27.3216615625) ; num. days in sidereal month (fixed star to fixed star)
	 (nakshatra-day (/ nakshatra-month 27)) ; 27 nak. in a month
	 (nak0 8) ; ashlesha on epoch, nak. counting from 0
	 (sun ; sunrise on that date
	  (+ (- fixed epoch) (hindu-calendar--fracday 6))))
    (1+ (mod (+ nak0 (floor sun nakshatra-day)) 27)))) ; nak. since epoch

;;----------------- new code -------------

(defun hindu-calendar--normalize-degrees (degrees)
  "Normalize DEGREES to the 0-360 range."
  (mod degrees 360.0))

(defun hindu-calendar--get-sunrise-ut (gregorian-date)
  "Calculate sunrise in UT for GREGORIAN-DATE using Emacs location settings.
Defaults to Ujjain, India if calendar coordinates are unset.
Falls back to UTC 0h if no sunrise exists (e.g., polar night/day)."
  (let* ((calendar-latitude (if (bound-and-true-p calendar-latitude)
                                calendar-latitude
                              23.1765))
         (calendar-longitude (if (bound-and-true-p calendar-longitude)
                                 calendar-longitude
                               75.7885))
         (calendar-time-zone (if (bound-and-true-p calendar-time-zone)
                                 calendar-time-zone
                               330)) ; +5:30 IST in minutes from UTC 0

         (sun-data (solar-sunrise-sunset gregorian-date))
         (sunrise-local (and sun-data (car (car sun-data)))))

    (if (numberp sunrise-local)
        (- sunrise-local (/ calendar-time-zone 60.0))
      0.0))) ; Midnight UTC 00:00

(defun hindu-calendar--get-solar-longitude (gregorian-date ut-hour &optional tropical-p)
  "Get true ecliptic longitude for a Gregorian DATE (month day year) and UT hour.
If TROPICAL-P is nil, returns Nirayana (sidereal) longitude using Lahiri Ayanamsha.
Otherwise, returns Sayana (tropical) longitude."
  (let* ((julian-centuries (solar-date-to-et gregorian-date ut-hour))
         (coords (solar-ecliptic-coordinates julian-centuries t))
         (tropical-long (car coords)))
    (if tropical-p
        (hindu-calendar--normalize-degrees tropical-long)
      ;; Apply linear approximation of Lahiri Ayanamsha
      ;; Base Ayanamsha at J2000.0 (Jan 1, 2000) is ~23.85°
      ;; Average rate of precession is ~1.39604° per Julian century
      (let* ((ayanamsha (+ 23.85709235 (* julian-centuries 1.39604)))
             (sidereal-long (- tropical-long ayanamsha)))
        (hindu-calendar--normalize-degrees sidereal-long)))))

(defun hindu-calendar--solar-from-gregorian (year month day &optional tropical-p)
  "Calculate the Sayana or Nirayana month and day for a Gregorian date.
Evaluates solar longitude at the exact moment of local sunrise (defaults to Ujjain).
If TROPICAL-P is nil, calculates against the Nirayana (sidereal) framework."
  (let* ((gregorian-date (list month day year))
         (target-abs-date (calendar-absolute-from-gregorian gregorian-date))

         ;; 1. Fetch Sunrise UT
         (target-ut-hour (hindu-calendar--get-sunrise-ut gregorian-date))

         ;; 2. Get current longitude at sunrise and determine the Rasi index
         (current-long (hindu-calendar--get-solar-longitude gregorian-date target-ut-hour tropical-p))
         (target-rasi-idx (floor (/ current-long 30.0)))

         ;; 3. Calculate Solar Kali Yuga Epoch
         (base-ky (+ year 3100))
         (ky-year (cond
                   ((<= month 2) base-ky)       ;; Jan-Feb: Always old year
                   ((>= month 5) (1+ base-ky))  ;; May-Dec: Always new year
                   ;; March-April: Check if Sun is still in late winter Rasis
                   (t (if (>= target-rasi-idx 9) base-ky (1+ base-ky)))))

         ;; 4. Search backward at sunrise each day to find the Sankranti transition
         (sankranti-abs-date target-abs-date)
         ;; Increased search limit to 35 safely covers maximum Nirayana month length
         ;; Though a tropical solar month is normally 29-32 days
         (search-limit (- target-abs-date 35)))

    (while (and (> sankranti-abs-date search-limit)
                (let* ((test-greg (calendar-gregorian-from-absolute sankranti-abs-date))
                       (test-ut (hindu-calendar--get-sunrise-ut test-greg))
                       (test-long (hindu-calendar--get-solar-longitude test-greg test-ut tropical-p)))
                  (= (floor (/ test-long 30.0)) target-rasi-idx))) ; one rasi is 30 degrees
      (setq sankranti-abs-date (1- sankranti-abs-date)))

    ;; 5. Calculate the day (Sankranti day itself is Day 1)
    (let* ((day-one sankranti-abs-date)
           (solar-day (1+ (- target-abs-date day-one)))
           (solar-month (1+ target-rasi-idx)) ; 0-11 index to 1-12 month
           (solar-year ky-year))
      (list solar-year solar-month solar-day))))

(defun hindu-calendar--sidereal-solar-from-gregorian (year month day)
  "Return sidereal solar date for given Gregorian (YEAR MONTH DAY)."
  (hindu-calendar--solar-from-gregorian year month day nil))

(defun hindu-calendar--tropical-solar-from-gregorian (year month day)
    "Return tropical solar date for given Gregorian (YEAR MONTH DAY)."
    (hindu-calendar--solar-from-gregorian year month day t))

(defun hindu-calendar--get-lunar-longitude (gregorian-date ut-hour &optional tropical-p)
  "Calculate true ecliptic longitude of the Moon using Meeus algorithms.
If TROPICAL-P is non-nil, returns Nirayana (sidereal) longitude using Lahiri Ayanamsha."
  (let* ((T (solar-date-to-et gregorian-date ut-hour))

         ;; 1. Fundamental Arguments (in degrees)
         (Lprime (hindu-calendar--normalize-degrees (+ 218.3164477 (* 481267.88123421 T))))
         (D      (hindu-calendar--normalize-degrees (+ 297.8501921 (* 445267.1114034 T))))
         (M      (hindu-calendar--normalize-degrees (+ 357.5291092 (* 35999.0502909 T))))
         (Mprime (hindu-calendar--normalize-degrees (+ 134.9633964 (* 477198.8675055 T))))
         (F      (hindu-calendar--normalize-degrees (+ 93.2720950  (* 483202.0175233 T))))

         ;; 2. Sum of major periodic terms for longitude (converted directly to degrees)
         (sigma-l (+
                   (* 6.288774 (solar-sin-degrees Mprime))
                   (* 1.274027 (solar-sin-degrees (- (* 2 D) Mprime)))
                   (* 0.658314 (solar-sin-degrees (* 2 D)))
                   (* 0.213618 (solar-sin-degrees (* 2 Mprime)))
                   (* -0.185116 (solar-sin-degrees M))
                   (* -0.114332 (solar-sin-degrees (* 2 F)))
                   (* 0.058793 (solar-sin-degrees (- (* 2 D) (* 2 Mprime))))
                   (* 0.057066 (solar-sin-degrees (- (* 2 D) (+ M Mprime))))
                   (* 0.053322 (solar-sin-degrees (+ (* 2 D) Mprime)))
                   (* 0.045758 (solar-sin-degrees (- (* 2 D) M)))
                   (* -0.040923 (solar-sin-degrees (- Mprime (* 2 F))))
                   (* -0.034720 (solar-sin-degrees D))
                   (* -0.030383 (solar-sin-degrees (+ Mprime (* 2 F))))))

         ;; 3. Tropical Longitude
         (tropical-long (hindu-calendar--normalize-degrees (+ Lprime sigma-l))))

    ;; 4. Apply Ayanamsha for Sidereal if requested
    (if tropical-p
        tropical-long
      (let* ((ayanamsha (+ 23.85709235 (* T 1.39604)))
             (sidereal-long (- tropical-long ayanamsha)))
        (hindu-calendar--normalize-degrees sidereal-long)))))

(defun hindu-calendar--lunar-from-gregorian (year month day &optional tropical-p)
  "Calculate the lunar month, Paksha, Spashta Tithi, and Kali Yuga epoch at local sunrise.
If TROPICAL-P is nil, calculates using Nirayana solar longitude."
  (let* ((target-date (list month day year))
         (target-abs (calendar-absolute-from-gregorian target-date))

         ;; 1. Fetch Sunrise UT and absolute time
         (target-ut-hour (hindu-calendar--get-sunrise-ut target-date))
         (target-abs-time (+ target-abs (/ target-ut-hour 24.0)))

         ;; 2. Calculate Exact Spashta Tithi at Sunrise
         (sun-long-sunrise (hindu-calendar--get-solar-longitude target-date target-ut-hour tropical-p))
         (moon-long-sunrise (hindu-calendar--get-lunar-longitude target-date target-ut-hour tropical-p))
         (elongation (hindu-calendar--normalize-degrees (- moon-long-sunrise sun-long-sunrise)))
         (tithi-idx (1+ (floor (/ elongation 12.0)))) ; 0-29 => 1-30

         ;; 3. Find bounding New Moons for Lunar Month
         ;; Sunrise times are in UTC, so new moon times must also be anchored to UTC
         (target-jd (calendar-astro-from-absolute target-abs-time))
         (prev-nm-jd (hindu-calendar--get-new-moon-ut (- target-jd tithi-idx)))
         (next-nm-jd (hindu-calendar--get-new-moon-ut (+ prev-nm-jd 1))))

    (let* ((prev-nm (calendar-astro-to-absolute prev-nm-jd))
           (next-nm (calendar-astro-to-absolute next-nm-jd))

           ;; 4. Evaluate Sun's longitude at both New Moon boundaries
           (nm1-abs-date (truncate prev-nm))
           (nm1-ut-hour (* (- prev-nm nm1-abs-date) 24.0))
           (nm1-greg-date (calendar-gregorian-from-absolute nm1-abs-date))
           (sun-long-start (hindu-calendar--get-solar-longitude nm1-greg-date nm1-ut-hour tropical-p))

           (nm2-abs-date (truncate next-nm))
           (nm2-ut-hour (* (- next-nm nm2-abs-date) 24.0))
           (nm2-greg-date (calendar-gregorian-from-absolute nm2-abs-date))
           (sun-long-end (hindu-calendar--get-solar-longitude nm2-greg-date nm2-ut-hour tropical-p))

           ;; 5. Determine Lunar Month and Adhika flag
           (rasi-idx-start (floor (/ sun-long-start 30.0)))
           (rasi-idx-end (floor (/ sun-long-end 30.0)))
           (is-adhika (= rasi-idx-start rasi-idx-end))
           ; lunar new year stars when Sun is in Meena Rasi aka Vaisakha (2)
           (lunar-month (+ 2 rasi-idx-start))

           ;; 6. Calculate Lunar Kali Yuga Epoch
           (base-ky (+ year 3101))
           (ky-year (if (and (>= rasi-idx-start 7)
                             (<= rasi-idx-start 10)
                             (<= month 5))
                        (1- base-ky)
                      base-ky)))

      (if (string= "purnimanta" (downcase hindu-calendar-lunar-type))
          (hindu-calendar--convert-to-purnimanta ky-year lunar-month is-adhika tithi-idx)
        (list ky-year lunar-month is-adhika tithi-idx))))) ; default is amanta

; wrappers for above
(defun hindu-calendar--sidereal-lunar-from-gregorian (year month day)
  "Return sidereal lunar date (YEAR MONTH LEAP-MONTH-P DAY), given Gregorian (YEAR MONTH DAY)"
    (hindu-calendar--lunar-from-gregorian year month day nil))

(defun hindu-calendar--tropical-lunar-from-gregorian (year month day)
  "Return tropical lunar date (YEAR MONTH LEAP-MONTH-P DAY), given Gregorian (YEAR MONTH DAY)"
  (hindu-calendar--lunar-from-gregorian year month day t))

(defun hindu-calendar--nakshatra-from-gregorian (year month day)
  "Calculate the nakshatra of the day."
  (let* ((target-date (list month day year))
         (target-abs (calendar-absolute-from-gregorian target-date))

         ;; 1. Fetch Sunrise UT and absolute time
         (target-ut-hour (hindu-calendar--get-sunrise-ut target-date))
         (target-abs-time (+ target-abs (/ target-ut-hour 24.0)))

         ;; 2. Calculate exact longitude at Sunrise (sidereal = true)
         (moon-long-sunrise (hindu-calendar--get-lunar-longitude target-date target-ut-hour nil))

         ;; 3. Twenty-seven nakshatras span 360 degrees
         (one-star (/ 360.0 27.0)))

    (ceiling moon-long-sunrise one-star)))

;;----------------------------- PROVIDE PACKAGE ---------------------------------
(provide 'hindu-calendar)
;;; hindu-calendar.el ends here

(defun set-bangalore ()
  (setq calendar-longitude 77.5775)
  (setq calendar-latitude 12.9629)
  (setq calendar-time-zone 330)
)


(defun set-espoo ()
  (setq calendar-longitude 24.65)
  (setq calendar-latitude 60.2)
  (setq calendar-time-zone 120)
)

(defun set-ujjain ()
  (setq calendar-longitude 75.7885)
  (setq calendar-latitude 23.1765)
  (setq calendar-time-zone 330)
  (setq calendar-daylight-time-offset 0)
)

(defun hindu-calendar--get-new-moon-ut (jd)
  "Calculate the Julian Day of the New Moon on or after JD.
Forces Universal Time (UT) by neutralizing local calendar timezone variables."
  (let ((calendar-time-zone 0)
        (calendar-daylight-savings-starts nil)
        (calendar-daylight-savings-ends nil)
        (calendar-daylight-time-offset 0))
    ; below function uses daylight savings, etc. based on TZ variable or emacs' environment
    (lunar-new-moon-on-or-after jd)))
