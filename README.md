# hindu-calendar

An observational Hindu calendar (drik-panchanga) for Emacs.

This package provides traditional Hindu calendars (solar and lunar) using
analytic equations (Meeus-style) based on astronomical motions of the Sun and
Moon. It calculates tithi and nakshatra. It provides both tropical (sayana) and
sidereal (nirayana/Chitrapaksha) variants. Lunar calendar can be chosen between
amanta or purnimanta.  Default location is Ujjain, but can be customized.

Older versions of this package used to provid arithmetical Hindu calendar, which
have been retired because the present astronomical version gives better
accuracy.

## Installation

This package is available on MELPA, do `M-x package-install RET hindu-calendar`.
If you want to install it manually (example):

```shell
git clone https://github.com/bdsatish/hindu-calendar ~/.emacs.d/
```

and add these lines your `~/.emacs` (or equivalent init file):

```lisp
(add-to-list 'load-path "~/.emacs.d/hindu-calendar/")
(require 'hindu-calendar)
(hindu-calendar-keybindings) ; Optional key bindings
```

## Usage

All of the functions can be called interactively or programmatically.

| Calendar type           | Function/command                  |
|-------------------------|-----------------------------------|
| Sidereal lunar          | M-x hindu-calendar-sidereal-lunar |
| Tropical lunar          | M-x hindu-calendar-tropical-lunar |
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
"Ashadha-01, 5131"
ELISP> (hindu-calendar-asterism 2030 6 21)
"Satabhishaj"
```

`S` and `K` denote _ṡukla-pakṣa_ and _kṛṣṇa-pakṣa_ respectively. For example,
K06 above means _kṛṣṇa-pakṣa-ṣaṣṭhī_. Years are counted as elapsed Kali-yuga
(_gatakali_; not present Kali-yuga years).

The lunar calendars by default follow _amānta_ (or _amāvāsyānta_) scheme, i.e.,
a month ends on new-moon day. This is the scheme used mostly in the
non-Hindi speaking parts of India. Much of North India uses _pūrṇimānta_, i.e.,
a month ends on full-moon day. [See here for
explanation](https://www.drikpanchang.com/faq/faq-ans8.html).

## Customization

Tithi is normally determined by the phase of the moon at the time of sunrise at
a given location. This package re-uses the following options provided by Emacs'
built-in `calendar` package:

- `calendar-latitude`: latitude of the desired location.
- `calendar-longitude`: longitude of the desired location.
- `calendar-time-zone`: time zone (in minutes from UTC) of the desired location.

If these are not customized by the user, then the location defaults to Ujjain.

All the below options can be customized via `M-x customize-group RET hindu-calendar`.

By default, the month names use `"Chaitra"`, Vaisakha, etc. Other options are:

- `"Mesha"` for Mesha, Vrishabha, etc. which are zodiacal names.
- `"Madhu"` for Madhu, Madhava, etc. which are ancient Vedic names.
- `"Kesava"` for Kesava, Narayana, etc. which are the twelve names of Lord Vishnu.
- `"Dhata"` for Dhātā, Aryamā, etc. which are the names of
   [twelve Ādityas](http://www.harekrsna.de/surya/12adityas.htm).
- `"Baisakha"` as first month of new solar year, as in Bengal, Assam, Odisha, etc.

Customize variable `hindu-calendar-month-type` to one of the above.

By default, the years are counted as elapsed `"Kali"`-yuga years. Other options are:

- `"Vikrama"` for Vikrama (or Bikram) samvat, whose epoch is 57 BCE.
- `"Saka"` for Salivahana saka, whose epoch is 78 CE.
- `"Kali"` for elapsed Kali varsha, whose epoch is 3102 BCE (i.e., -3101).
- `"Bengali"` for Bengali san, whose epoch is 593 CE.

Customize variable `hindu-calendar-epoch-type` to one of the above.

By default, `"amanta"` new-moon based lunar calendar is used. Customize variable
`hindu-calendar-lunar-type` to `"purnimanta"` for full-moon based reckoning.

Here are some typical combinations:

| Category                      | `hindu-calendar-` | `hindu-calendar-month-type` | `hindu-calendar-epoch-type` |
|:------------------------------|-------------------|-----------------------------|-----------------------------|
| [Indian national calendar][1] | indian-national   | chaitra                     | saka                        |
| Kannadiga, Marathi, Telugu    | sidereal-lunar    | chaitra                     | saka                        |
| Gujarati                      | sidereal-lunar    | chaitra                     | vikrama (kārttikādi)        |
| Tamizhan                      | sidereal-solar    | chaitra                     | saka, kali                  |
| Malayali                      | sidereal-solar    | mesha                       | kollam                      |
| Bengali                       | sidereal-solar    | baisakha                    | bengali                     |
| Odia                          | sidereal-solar    | baisakha                    | bengali, vilayati           |
| Nepali, Punjabi               | sidereal-solar    | baisakha                    | vikrama                     |
| Rest of India (purnimanta)    | sidereal-lunar    | chaitra                     | vikrama                     |
| Vedic (Vajasaneyi/Taittiriya) | tropical-lunar    | madhu                       | N/A                         |

[1]: https://en.wikipedia.org/wiki/Indian_national_calendar

Of course, each language has its own names for the months (e.g., Agraha(ya)n,
Margazhi, etc. for Margasirsa) but it is hoped that the above is indicative
enough.

It is also integrated with Emacs' calendar. Type `M-x calendar` to open-up 3-month
calendar view. Move cursor (point) over any date you want. Type `p H` (uppercase H)
to print Hindu calendar in the echo area. Here's an example with settings for
someone in Mathura or Varanasi, where Purnimanta/Vikrama era is the norm:

[Sample screenshot](screenshot.jpg):

![Sample screenshot](screenshot.png "Hindu Panchanga")

The tropical solar calendar has 29 to 32 days depending on the month. Normally,
six months from Vaishkha onwards have 31 days and the rest six have 30 days, but
this pattern is not fixed and changes yearly. There is no concept of a 'leap'
year.

## Accuracy

This program is about as accurate as your traditional siddhantic panchangas or
[drik panchanga](https://drikpanchanga.com). That is, it differs by a maximum of
±1 lunar day (_tithi_) or ±1 nakshatra compared to the true values. Though rare,
an _adhika-māsa_ (leap month) can be off by ±1 month too. Solar calendars are
more accurate than lunar calendars (because moon's ecliptic longitude is not as
precise as for the sun). Meeus-style formulas for ecliptic longitudes limit the
supported dates to be about 1900 - 2100 CE for best results. Dates outside this
range may work, but tend to be erroneous.

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

## Bugs

Yes, many. Please [report a new issue](https://github.com/bdsatish/hindu-calendar/issues).

## Contributions

By submitting any snippet of code to this project, you agree with the [Developer
Certificate of Origin](https://developercertificate.org/) by adding a line:

    Signed-off-by: Anamika Ashok <nameless@example.org>

using your real name. Use `git commit -s` to do this automatically.

## Licence

Copyright (C) B.D.Satish. GNU Affero GPL v3 (or later).

### Note

The so-called "Vedic astrology" has no basis in the Vedas, Upanishads, Bhagavad
Gita, Mahabharata or Ramayana. It is a fringe science of Hinduism. The original
[Vedanga Jyotisha](https://archive.org/details/VedangaJyotisa) (~1200 BCE) and
[Surya Siddhanta](https://archive.org/details/in.ernet.dli.2015.69065)
(~400 CE) are purely astronomical.

<!--
[Dieter Koch's](https://www.gilgamesh.ch/kalender/kalender.html) website uses
better approximations to the true motions.
-->
