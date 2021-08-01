# bittermelon

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/bittermelon)](https://cran.r-project.org/package=bittermelon)
[![R-CMD-check](https://github.com/trevorld/bittermelon/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/bittermelon/actions)
[![Coverage Status](https://img.shields.io/codecov/c/github/trevorld/bittermelon.svg)](https://codecov.io/github/trevorld/bittermelon?branch=main)
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Related Software](#similar)

## <a name="overview">Overview</a>

**This project is currently at the 'Concept' level and is not currently usable.**  Check out the awesome [{bdftools}](https://github.com/coolbutuseless/bdftools) package if you currently need bitmap font support in R.  `{bdftools}` will probably always provide cooler bitmap font graphical output support than `{bittermelon}`.  The primary goal of `{bittermelon}` will be to make it easier for me to create new bitmap fonts using R.

`{bittermelon}` Design Goals:

* Provide functions for creating and modifying bitmap font glyphs.  

  * Font glyphs will simply be integer matrices with special methods

    * `print()` method so can see glyphs in terminal
    * `as.raster()` method so can generate some (crude) graphics
    * crop, trim, pad, expand methods
    * merge and difference methods (perhaps use `+` and `-`)
    * generate glyph by capturing results of drawing to an R graphics device
    * perhaps bold, shadow, glow effects?
    * rotate?

* Provide native read/write support for the 'hex' and 'yaff' bitmap font formats 
  
  * I'm not smart enough to fully grok the 'bdf' font format but the 'hex' and 'yaff' font formats are simpler alternatives I can understand: basically each Unicode glyph is simply associated with an encoding of a matrix of ones and zeroes.
  * Fonts will simply be named lists of font glyphs with special methods.  

    * Names should be of the form "U+HEX" where "HEX" is the Unicode hexadecimal number for the character.  This format is understood well by the `{Unicode}` package.  Note `Unicode::as.u_char()` does not seem to support any string format that is reliably a "syntactic name" identifier.
    * Font metadata will be stored as an attribute.  Note "hex" font format has **zero** font metadata so this should be optional.

* If 'Python' is installed will also read/write several more bitmap font formats using an embedded version of [monobit](https://github.com/robhagemans/monobit).

Personal Project Goals:

* Generate enough bitmap manipulation capabilities in order to create a specialized monoscale font to support a [boardgame diagram use case no existing Unicode font seems to support well](https://trevorldavis.com/piecepackr/unicode-piecepack-diagrams.html#piecepack-font-wishlist).

  * A 16x16 "hex" font could be a good initial prototype target.

* Enable people to render such board game diagrams using the font in R (and perhaps XeLaTeX or HTML) even if the user doesn't have the above font installed.  Ideally the rendering support must support Unicode "combining" characters, ideally `{crayon}` coloring, and ideally "compression" using "Block characters".
* Generate ability to efficiently store and use a few bitmap icons in `{piecepackr}` to provide a "configuration" users could use to generate certain game icons if they don't have the right fonts installed on their system for more "natural" font based approaches.

  * As an alternative don't want to bundle a bunch of large images with `{piecepackr}`
  * In particular "sans" lacks Chess symbols and certain Piecepack symbols.  Additionally "Dejavu Sans" lacks good Xiangqi and Shogi symbols.  Certain games (we might never support) such as Arimaa and Hive could be straightforwardly implemented with various Emoji animal glyphs.
  * `{bittermelon}` must eventually be made available on CRAN for a builtin configuration to be usable by `{piecepackr}`.

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/bittermelon")
```

## <a name="examples">Examples</a>



To be completed...

## <a name="similar">Related Software</a>

R packages:

* [bdftools](https://github.com/coolbutuseless/bdftools) provides some tools for reading, manipulating, and outputting BDF bitmap fonts.  In particular provides much richer graphical output capabilities than `{bittermelon}` will likely support including a specialized `{ggplot2}` geom.  It also contains the `Cozette`, `Creep2`, and `Spleen` bitmap fonts. 
* [fontr](https://github.com/yixuan/fontr) allows one to extract character glyphs from a specific font (which itself may not be a bitmap font) as a bitmap.

Python:

* [BDF Parser Python library](https://github.com/tomchen/bdfparser)
* [bdflib](https://gitlab.com/Screwtapello/bdflib)
* [monobit](https://github.com/robhagemans/monobit) lets one modify bitmap fonts and convert between several formats.  Embedded within `{bittermelon}`.

Other:

* [Bits'n'Picas](https://github.com/kreativekorp/bitsnpicas)
* [FontForge](http://fontforge.org/en-US/)

Fonts:

* [GNU Unifont](https://www.unifoundry.com/unifont/index.html)
* Rob Hagerman's [Hoard of Bitfonts](https://github.com/robhagemans/hoard-of-bitfonts)
* Jacob Large's [bitmap-fonts](https://github.com/Tecate/bitmap-fonts)
* Embedded in `{bdftools}` package:

  * [Cozette](https://github.com/slavfox/Cozette)
  * [Creep2](https://github.com/raymond-w-ko/creep2) 
  * [Spleen](https://github.com/fcambus/spleen)
