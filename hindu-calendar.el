;;; hindu-calendar.el --- Astronomical traditional Hindu calendar (panchanga)  -*- lexical-binding:t -*-

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
;; analytic equations (Meeus-style) based on astronomical motions of the Sun and
;; Moon.  It calculates tithi and nakshatra.  It provides both tropical (sayana)
;; and sidereal (nirayana/Chitrapaksha) variants.  Lunar calendar can be chosen
;; between amanta or purnimanta.  Default location is Ujjain, which can be
;; customized via `calendar-latitude', `calendar-longitude' and
;; `calendar-timezone'
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

(defun hindu-calendar--normalize-degrees (degrees)
  "Normalize DEGREES to the 0-360 range."
  (mod degrees 360.0))

(defun hindu-calendar--get-new-moon-ut (jd)
  "Calculate the Julian Day of the New Moon on or after JD.
Forces Universal Time (UT) by neutralizing local calendar timezone variables."
  (let ((calendar-time-zone 0)
        (calendar-daylight-savings-starts nil)
        (calendar-daylight-savings-ends nil)
        (calendar-daylight-time-offset 0))
    ; below function uses daylight savings, etc. based on TZ variable or emacs' environment
    (lunar-new-moon-on-or-after jd)))

(defun hindu-calendar--convert-to-purnimanta (year month leap? day)
  "Convert given amanta date (`YEAR' `MONTH' `LEAP?' `DAY') to purnimanta."
  (if (and (> day 15) (not leap?)) ; bump month only for krishna paksha
      (list year (1+ (mod month 12)) leap? day)
    (list year month leap? day)))

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
         (calendar-daylight-time-offset (if (bound-and-true-p calendar-daylight-time-offset)
                                            calendar-daylight-time-offset
                                          0)) ; No Daylight Savings Time in India
         (calendar-time-zone (if (bound-and-true-p calendar-time-zone)
                                 calendar-time-zone
                               330)) ; UTC+5:30 IST in minutes

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
         (target-rasi-idx (/ (ceiling current-long) 30))

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
