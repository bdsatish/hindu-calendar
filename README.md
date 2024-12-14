# hindu-calendar

A simplified arithmetical Hindu calendar (panchanga) for Emacs.

This package provides traditional Hindu calendars (solar and lunar) using
arithmetic based on the _mean_ motions of the Sun and Moon. It provides both
tropical (sāyana) and sidereal (nirayana/Lahiri ayanāmsa) variants. It
calculates tithi and nakshatra as well.

## Installation

This package is available on MELPA, do `M-x package-install hindu-calendar`.
If you want to install it manually (example):

```shell
mkdir ~/.emacs.d/lisp/ && cd $_
git clone https://github.com/bdsatish/hindu-calendar
```

and add these lines your `~/.emacs` (or equivalent init file):

```lisp
(add-to-list 'load-path "~/.emacs.d/lisp/hindu-calendar")
(require 'hindu-calendar)
```

## Usage

All of the functions can be called interactively or programmatically.

| Calendar type           | Function/command                  |
|-------------------------|-----------------------------------|
| Sidereal lunar (amānta) | M-x hindu-calendar-sidereal-lunar |
| Tropical lunar (amānta) | M-x hindu-calendar-tropical-lunar |
| Sidereal solar          | M-x hindu-calendar-sidereal-solar |
| Tropical solar          | M-x hindu-calendar-tropical-solar |
| Nakshatra (sidereal)    | M-x hindu-calendar-asterism       |

If you are not sure which one to use, then start with the first one:
`(hindu-calendar-sidereal-lunar)`

All of these functions accept year, month and date as arguments. If none are
given, then today's values are used.

```
ELISP> (hindu-calendar-sidereal-lunar 2030 6 21)
"Jyaishtha-K06, 5131"
ELISP> (hindu-calendar-tropical-lunar 2030 6 21)
"Ashadha-K06, 5131"
ELISP> (hindu-calendar-sidereal-solar 2030 6 21)
"Jyaishtha-07, 5131"
ELISP> (hindu-calendar-tropical-solar 2030 6 21)
"Jyaishtha-31, 5131"
ELISP> (hindu-calendar-asterism 2030 6 21)
"Satabhishaj"
```

`S` and `K` denote _ṡukla-pakṣa_ and _kṛṣṇa-pakṣa_ respectively. For example,
K06 above means _kṛṣṇa-pakṣa ṣaṣṭhī_.

Negative years are supported too, as per [proleptic Gregorian
calendar](https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar).

The lunar calendars by default follow _amānta_ (or _amāvāsyānta_) scheme, i.e.,
a new month starts after new-moon day. This is the scheme used mostly in the
non-Hindi speaking parts of India. Much of North India uses _pūrṇimānta_, i.e.,
months start after full-moon day. [See here for
explanation](https://www.drikpanchang.com/faq/faq-ans8.html).

## Customizations

By default, the month names use `Chaitra`, Vaisakha, etc. Other options are:

- `Mesha` for Mesha, Vrishabha, etc. which are zodiacal names.
- `Madhu` for Madhu, Madhava, etc. which are old Vedic names.
- `Kesava` for Kesava, Narayana, etc. which are the twelve names of Lord Vishnu.
- `Dhata` for Dhātā, Aryamā, etc. which are the names of
   [twelve Ādityas](http://www.harekrsna.de/surya/12adityas.htm).

By default, the years are counted as elapsed Kali-yuga years. Other options are:

- `Vikrama` for Vikrama (or Bikram) samvat, whose epoch is 57 BCE.
- `Saka` for Salivahana saka, whose epoch is 78 CE.
- `Kali` for elapsed Kali varsha, whose epoch is 3102 BCE (i.e., -3101).
- `Bengali` for Bengali san, whose epoch is 593 CE.

## Accuracy

This program is about as accurate as your traditional siddhantic panchangas.
That is, it differs by a maximum of ±1 lunar day (_tithi_) or ±1 solar day or ±1
nakshatra compared to the true values. Though rare, an _adhika-māsa_ (leap
month) can be off by ±1 month too. Any dates in the range of ±5000 years are
supported.

Calendrical calculations are of two types: arithmetical and astronomical.
Arithmetical calendars are simpler to understand (and implement!) because they
use [mean motion](https://en.wikipedia.org/wiki/Mean_motion) (i.e. average
values) of the Sun and Moon. They are crude approximations to the _true motion_,
used by astronomical calendars.

The biggest advantage of arithmetical calendars is that they can be computed
independent of location, by assuming a reasonable time of sunrise (e.g. 6:00
A.M.). Astronomical calendars change their dates based on one's location and
sunrise timings. The Python program
[drik-panchanga](https://github.com/bdsatish/drik-panchanga) is astronomical
and thus gives best accuracy.

## License

Copyright (C) B.D.Satish. GNU Affero GPL v3 (or later).

<!--
[Dieter Koch's](https://www.gilgamesh.ch/kalender/kalender.html) website uses
better approximations to the true motions.
-->