;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'hindu-calendar)

(defmacro assert-lists-equal (l1 l2)
    `(cl-assert (not (cl-set-difference ,l1 ,l2)))
)

(defun to-fixed (year month day) (calendar-absolute-from-gregorian (list month day year)))
(defun from-fixed  (fixed) (-rotate 1 (calendar-gregorian-from-absolute fixed)))

(defun set-bangalore ()
  (setq calendar-longitude 77.5775)
  (setq calendar-latitude 12.9629)
  (setq calendar-time-zone 330)
)

(defun set-northern-latitudes ()
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

(defun hindu-calendar-tests ()
  (let ((hindu-calendar-epoch-type "kali") ; reset to default values
	(hindu-calendar-month-type "chaitra")
	(hindu-calendar-lunar-type "amanta")
        ; in 285 C.E. both tropical and sidereal zodiacs overlap on vernel equinox
        (zero-point-lahiri (list +285 3 21))
       )
    (assert-lists-equal (apply #'hindu-calendar--sidereal-solar-from-gregorian zero-point-lahiri)
                        (apply #'hindu-calendar--tropical-solar-from-gregorian zero-point-lahiri))
    (assert-lists-equal (apply #'hindu-calendar--sidereal-lunar-from-gregorian zero-point-lahiri)
                        (apply #'hindu-calendar--tropical-lunar-from-gregorian zero-point-lahiri))
    (assert-lists-equal (hindu-calendar--sidereal-solar-from-gregorian 2024 12 6)
                        '(5125 8 21)) ; Vrishchika (Malylm.), Ogrohaeon (Bengali), Margashira (Odia)
    (assert-lists-equal (hindu-calendar--tropical-solar-from-gregorian 2024 12 6)
                        '(5125 9 16)) ; Margashira (INC), sayana Dhanus
    (assert-lists-equal (from-fixed (to-fixed 2024 12 6)) '(2024 12 6))
    (assert-lists-equal (from-fixed (to-fixed +5238 12 28)) '(5238 12 28))

    ; lunar calendar tests
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2024 12 7 t)
                        '(5125 10 nil 6)) ; sayana Pushya S6
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2024 12 7 nil)
                        '(5125 9 nil 6)) ; nirayana Margashira S6

    ; around new years. Solar calendars are usual 21-Mar (equinox) and 14-Apr (Tamil new year).
    ; Lunar calendar varies. Year switches from 5124->5125 when month switches from 12->1
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2024 4 8 nil)
                        '(5124 12 nil 30))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2024 4 9 nil)
                        '(5125 1 nil 1))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2024 3 10 t)
                        '(5124 12 nil 30))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2024 3 11 t)
                        '(5125 1 nil 1))
    (assert-lists-equal (hindu-calendar--solar-from-gregorian 2024 3 19 t)
                        '(5124 12 30))
    (assert-lists-equal (hindu-calendar--solar-from-gregorian 2024 3 20 t)
                        '(5125 1 1))
    (assert-lists-equal (hindu-calendar--solar-from-gregorian 2024 4 12 nil)
                        '(5124 12 30))
    (assert-lists-equal (hindu-calendar--solar-from-gregorian 2024 4 13 nil)
                        '(5125 1 1))
    (assert-lists-equal (hindu-calendar--solar-from-gregorian 2026 4 13 nil)
                        '(5126 12 31))
    (assert-lists-equal (hindu-calendar--solar-from-gregorian 2024 4 14 nil)
                        '(5127 1 1))

    ; tropical around adhika-masa
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2001 7 2 t)
                        '(5102 5 t 12))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2023 4 10 t)
                        '(5124 2 t 19))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2023 5 10 t)
                        '(5124 2 nil 20))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2025 8 26 t)
                        '(5126 7 t 3))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2025 9 26 t)
                        '(5126 8 nil 4)) ; NOK, month must be 7
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2036 8 1 t)
                        '(5137 6 t 10))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2036 9 1 t)
                        '(5137 6 nil 11))
    ; lunar sidereal.
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2026 6 9 nil)
                        '(5127 3 t 24))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2026 7 9 nil)
                        '(5127 3 nil 24))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2029 4 10 nil)
                        '(5130 1 t 27))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2029 5 10 nil)
                        '(5130 1 nil 27))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2031 9 13 nil)
                        '(5132 6 t 27))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2031 10 13 nil)
                        '(5132 6 nil 27))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2034 6 25 nil)
                        '(5135 4 t 9))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2034 7 25 nil)
                        '(5135 4 nil 10))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2039 9 20 nil)
                        '(5140 7 t 2))
    (assert-lists-equal (hindu-calendar--lunar-from-gregorian 2039 10 20 nil)
                        '(5140 7 nil 3))

    ) ; end-let

    ; Frontend tests
    (let ((hindu-calendar-epoch-type "vikrama"))
      (cl-assert (string= "Margasirsa-S15, 2081" (hindu-calendar-sidereal-lunar 2024 12 15)))
      (cl-assert (string= "Margasirsa-01, 2081" (hindu-calendar-sidereal-solar 2024 12 15)))
      (cl-assert (string= "Pausha-S15, 2081" (hindu-calendar-tropical-lunar 2024 12 15)))
      (cl-assert (string= "Margasirsa-25, 2081" (hindu-calendar-tropical-solar 2024 12 15)))
    )

    (let ((hindu-calendar-epoch-type "saka"))
      (cl-assert (string= "Margasirsa-S15, 1946" (hindu-calendar-sidereal-lunar 2024 12 15)))
      (cl-assert (string= "Margasirsa-01, 1946" (hindu-calendar-sidereal-solar 2024 12 15)))
      (cl-assert (string= "Pausha-S15, 1946" (hindu-calendar-tropical-lunar 2024 12 15)))
      (cl-assert (string= "Margasirsa-25, 1946" (hindu-calendar-tropical-solar 2024 12 15)))
    )

    ; outside of "let" block, default is Kali era
    (cl-assert (string= "Margasirsa-S15, 5125" (hindu-calendar-sidereal-lunar 2024 12 15)))
    (cl-assert (string= "Margasirsa-01, 5125" (hindu-calendar-sidereal-solar 2024 12 15)))
    (cl-assert (string= "Pausha-S15, 5125" (hindu-calendar-tropical-lunar 2024 12 15)))
    (cl-assert (string= "Margasirsa-25, 5125" (hindu-calendar-tropical-solar 2024 12 15)))

    (let ((hindu-calendar-month-type "madhu"))
      (cl-assert (string= "Amhaspati-K09, 5127" (hindu-calendar-sidereal-lunar 2026 6 9)))
      (cl-assert (string= "Amhaspati-S03, 5126" (hindu-calendar-tropical-lunar 2025 8 26)))
    )

    (let ((hindu-calendar-month-type "kesava"))
      (cl-assert (string= "Purushottama-K09, 5127" (hindu-calendar-sidereal-lunar 2026 6 9)))
      (cl-assert (string= "Purushottama-S03, 5126" (hindu-calendar-tropical-lunar 2025 8 26)))
    )

    ; default is Chaitra-based months, adhika-masa occurs before nija-masa
    (cl-assert (string= "Vaisakha-K15, 5127" (hindu-calendar-sidereal-lunar 2026 5 16)))
    (cl-assert (string= "Adhika-Jyaishtha-S03, 5127" (hindu-calendar-sidereal-lunar 2026 5 19)))
    (cl-assert (string= "Adhika-Jyaishtha-K09, 5127" (hindu-calendar-sidereal-lunar 2026 6 9)))
    (cl-assert (string= "Jyaishtha-S11, 5127" (hindu-calendar-sidereal-lunar 2026 6 25)))
    (cl-assert (string= "Jyaishtha-K01, 5127" (hindu-calendar-sidereal-lunar 2026 6 30)))

    (cl-assert (string= "Adhika-Asvina-S03, 5126" (hindu-calendar-tropical-lunar 2025 8 26)))

    ; solar calendars must not be affected by leap-month setting
    (cl-assert (string= "Margasirsa-01, 5125" (hindu-calendar-sidereal-solar 2024 12 15)))
    (cl-assert (string= "Margasirsa-25, 5125" (hindu-calendar-tropical-solar 2024 12 15)))

    ; Purnimanta calendars. New year begins on Chaitra-S01 here also.
    (let ((hindu-calendar-lunar-type "purnimanta"))
      (cl-assert (string= "Phalguna-K11, 5124" (hindu-calendar-sidereal-lunar 2024 3 6)))
      (cl-assert (string= "Phalguna-S04, 5124" (hindu-calendar-sidereal-lunar 2024 3 13)))
      (cl-assert (string= "Chaitra-K12, 5124" (hindu-calendar-sidereal-lunar 2024 4 6))) ; year doesn't change
      (cl-assert (string= "Chaitra-S05, 5125" (hindu-calendar-sidereal-lunar 2024 4 13))) ; year changes

      ; with adhika masa sandwich'd between nija-jyestha
      (cl-assert (string= "Jyaishtha-K15, 5127" (hindu-calendar-sidereal-lunar 2026 5 16)))
      (cl-assert (string= "Adhika-Jyaishtha-S03, 5127" (hindu-calendar-sidereal-lunar 2026 5 19)))
      (cl-assert (string= "Adhika-Jyaishtha-K09, 5127" (hindu-calendar-sidereal-lunar 2026 6 9)))
      (cl-assert (string= "Jyaishtha-S11, 5127" (hindu-calendar-sidereal-lunar 2026 6 25)))
      (cl-assert (string= "Ashadha-K01, 5127" (hindu-calendar-sidereal-lunar 2026 6 30)))
    )

    (cl-assert (string= "Satabhishaj" (hindu-calendar-asterism 2030 6 21)))
) ; end-defun

; tropical lunisolar adhika-masa
; 02-Jul-2001, adhika-sravana-S12 ok
; 26-Aug-2025, adhika-ashvina-S3 ok
; 10-Apr-2023, adhika-vaisakha-K4 nok (gives normal vaisakha-k4)
; 01-Aug-2036, adhika-bhadrapada-S10 ok
;
; sidereal lunisolar adhika-masa
; 09-Jun-2026 adhika-jyestha-K10 ok
; 10-Apr-2029 adhika-caitra-K12 ok.
; 13-Sep-2031 adhika-bhadra-K12 ok
; 25-Jun-2034 adhika-asadha-S09 ok
; 20-Sep-2039 adhika-asvina-S02 ok
; 02/May/-3002 adhika-asadha-K11 nok
