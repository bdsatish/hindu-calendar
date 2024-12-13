;;; hindu-calendar.el --- Arithmetic Hindu calendar (panchanga) for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2025 B.D.Satish <bdsatish@gmail.com>

;; Author: B.D.Satish <bdsatish@gmail.com>
;; Maintainer: B.D.Satish <bdsatish@gmail.com>
;; Version: 0.0.1
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
;; tropical (sayana) and sidereal (nirayana/Lahiri) variants.

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


(provide 'hindu-calendar)

;;; hindu-calendar.el ends here
