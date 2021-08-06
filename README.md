# bittermelon

[![CRAN Status
Badge](https://www.r-pkg.org/badges/version/bittermelon)](https://cran.r-project.org/package=bittermelon)
[![R-CMD-check](https://github.com/trevorld/bittermelon/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/bittermelon/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/trevorld/bittermelon.svg)](https://codecov.io/github/trevorld/bittermelon?branch=main)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

### Table of Contents

-   [Overview](#overview)
-   [Installation](#installation)
-   [Examples](#examples)
-   [Future Goals](#future)
-   [Related Software](#similar)

## <a name="overview">Overview</a>

`{bittermelon}` provides functions for creating and modifying bitmaps
with special emphasis on bitmap fonts and their glyphs. It provides
native read/write support for the ‘hex’ and ‘yaff’ bitmap font formats
and if ‘Python’ is installed can also read/write several more bitmap
font formats using an embedded version of
[monobit](https://github.com/robhagemans/monobit).

**This project is currently a ‘Work-in-Progress’. Features are being
implemented and the API is not yet stable.** Check out the awesome
[{bdftools}](https://github.com/coolbutuseless/bdftools) package if you
currently need bitmap font support in R. `{bdftools}` will probably
always provide cooler bitmap font graphical output support than
`{bittermelon}`. The primary goal of `{bittermelon}` is to make it
easier for me to create new bitmap fonts using R.

## <a name="installation">Installation</a>

    remotes::install_github("trevorld/bittermelon")

The functions `read_monobit()` and `write_monobit()` that use on the
embedded version of [monobit](https://github.com/robhagemans/monobit)
require that Python is available on the system. A couple of the bitmap
font output formats supported by `write_monobit()` also require that the
“Pillow” or “reportlab” Python packages are installed (installable via
`pip3`).

## <a name="examples">Examples</a>

    library("bittermelon")
    font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
    font <- read_hex(font_file)
    bml <- as_bm_list("RSTATS", font = font)
    bm <- do.call(cbind, bml)
    print(bm)

    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
    ██████░░░██████░████████░█████░░████████░██████░
    ██░░░██░██░░░░░░░░░██░░░██░░░██░░░░██░░░██░░░░░░
    ██░░░██░██░░░░░░░░░██░░░██░░░██░░░░██░░░██░░░░░░
    ██░░░██░██░░░░░░░░░██░░░██░░░██░░░░██░░░██░░░░░░
    ██████░░░█████░░░░░██░░░███████░░░░██░░░░█████░░
    ██░░░██░░░░░░██░░░░██░░░██░░░██░░░░██░░░░░░░░██░
    ██░░░██░░░░░░██░░░░██░░░██░░░██░░░░██░░░░░░░░██░
    ██░░░██░░░░░░██░░░░██░░░██░░░██░░░░██░░░░░░░░██░
    ██░░░██░░░░░░██░░░░██░░░██░░░██░░░░██░░░░░░░░██░
    ██░░░██░██████░░░░░██░░░██░░░██░░░░██░░░██████░░
    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
    ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

    bml <- bml |> bm_pad(sides = 2L) |>
        bm_shadow() |>
        bm_extend(sides = c(2L, 1L), value = 3L)
    bm <- do.call(cbind, bml) |> bm_pad(sides = 2L, value = 3L)
    print(bm, px = px_ascii)

    ########################################################################################
    ########################################################################################
    ##------------##------------##-------------##------------##-------------##------------##
    ##------------##------------##-------------##------------##-------------##------------##
    ##--@@@@@@----##---@@@@@@---##--@@@@@@@@---##---@@@@@----##--@@@@@@@@---##---@@@@@@---##
    ##--@@+++@@---##--@@++++++--##---++@@++++--##--@@+++@@---##---++@@++++--##--@@++++++--##
    ##--@@+--@@+--##--@@+-------##-----@@+-----##--@@+--@@+--##-----@@+-----##--@@+-------##
    ##--@@+--@@+--##--@@+-------##-----@@+-----##--@@+--@@+--##-----@@+-----##--@@+-------##
    ##--@@@@@@++--##---@@@@@----##-----@@+-----##--@@@@@@@+--##-----@@+-----##---@@@@@----##
    ##--@@+++@@---##----+++@@---##-----@@+-----##--@@+++@@+--##-----@@+-----##----+++@@---##
    ##--@@+--@@+--##-------@@+--##-----@@+-----##--@@+--@@+--##-----@@+-----##-------@@+--##
    ##--@@+--@@+--##-------@@+--##-----@@+-----##--@@+--@@+--##-----@@+-----##-------@@+--##
    ##--@@+--@@+--##-------@@+--##-----@@+-----##--@@+--@@+--##-----@@+-----##-------@@+--##
    ##--@@+--@@+--##--@@@@@@++--##-----@@+-----##--@@+--@@+--##-----@@+-----##--@@@@@@++--##
    ##---++---++--##---++++++---##------++-----##---++---++--##------++-----##---++++++---##
    ##------------##------------##-------------##------------##-------------##------------##
    ##------------##------------##-------------##------------##-------------##------------##
    ########################################################################################
    ########################################################################################

    print(bm)

    ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
    ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
    ▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓
    ▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓
    ▓▓░░██████░░░░▓▓░░░██████░░░▓▓░░████████░░░▓▓░░░█████░░░░▓▓░░████████░░░▓▓░░░██████░░░▓▓
    ▓▓░░██▒▒▒██░░░▓▓░░██▒▒▒▒▒▒░░▓▓░░░▒▒██▒▒▒▒░░▓▓░░██▒▒▒██░░░▓▓░░░▒▒██▒▒▒▒░░▓▓░░██▒▒▒▒▒▒░░▓▓
    ▓▓░░██▒░░██▒░░▓▓░░██▒░░░░░░░▓▓░░░░░██▒░░░░░▓▓░░██▒░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░██▒░░░░░░░▓▓
    ▓▓░░██▒░░██▒░░▓▓░░██▒░░░░░░░▓▓░░░░░██▒░░░░░▓▓░░██▒░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░██▒░░░░░░░▓▓
    ▓▓░░██████▒▒░░▓▓░░░█████░░░░▓▓░░░░░██▒░░░░░▓▓░░███████▒░░▓▓░░░░░██▒░░░░░▓▓░░░█████░░░░▓▓
    ▓▓░░██▒▒▒██░░░▓▓░░░░▒▒▒██░░░▓▓░░░░░██▒░░░░░▓▓░░██▒▒▒██▒░░▓▓░░░░░██▒░░░░░▓▓░░░░▒▒▒██░░░▓▓
    ▓▓░░██▒░░██▒░░▓▓░░░░░░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░██▒░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░░░░░░██▒░░▓▓
    ▓▓░░██▒░░██▒░░▓▓░░░░░░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░██▒░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░░░░░░██▒░░▓▓
    ▓▓░░██▒░░██▒░░▓▓░░░░░░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░██▒░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░░░░░░██▒░░▓▓
    ▓▓░░██▒░░██▒░░▓▓░░██████▒▒░░▓▓░░░░░██▒░░░░░▓▓░░██▒░░██▒░░▓▓░░░░░██▒░░░░░▓▓░░██████▒▒░░▓▓
    ▓▓░░░▒▒░░░▒▒░░▓▓░░░▒▒▒▒▒▒░░░▓▓░░░░░░▒▒░░░░░▓▓░░░▒▒░░░▒▒░░▓▓░░░░░░▒▒░░░░░▓▓░░░▒▒▒▒▒▒░░░▓▓
    ▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓
    ▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓░░░░░░░░░░░░░▓▓░░░░░░░░░░░░▓▓
    ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
    ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

    # Can also print colored terminal output via suggested package {crayon}
    if (crayon::has_color())
        print(bm, px = " ", bg = c("white", "black", "grey", "red"))

    plot(bm, col = c("white", "darkblue", "lightblue", "black"))

![](man/figures/README-plot-1.png)

## <a name="future">Future Goals</a>

-   Generate enough bitmap manipulation capabilities in order to create
    a specialized monoscale font to support a [boardgame diagram use
    case no existing Unicode font seems to support
    well](https://trevorldavis.com/piecepackr/unicode-piecepack-diagrams.html#piecepack-font-wishlist).

    -   A 16x16 “hex” font could be a good initial prototype target.

-   Enable people to render such board game diagrams using the font in R
    (and perhaps XeLaTeX or HTML) even if the user doesn’t have the
    above font installed. Ideally the rendering support must support
    Unicode “combining” characters, ideally `{crayon}` coloring, and
    ideally “compression” using “Block characters”.

-   Generate ability to efficiently store and use a few bitmap icons in
    `{piecepackr}` to provide a “configuration” users could use to
    generate certain game icons if they don’t have the right fonts
    installed on their system for more “natural” font based approaches.

    -   As an alternative don’t want to bundle a bunch of large images
        with `{piecepackr}`
    -   In particular “sans” lacks Chess symbols and certain Piecepack
        symbols. Additionally “Dejavu Sans” lacks good Xiangqi and Shogi
        symbols. Certain games (we might never support) such as Arimaa
        and Hive could be straightforwardly implemented with various
        Emoji animal glyphs.
    -   `{bittermelon}` must eventually be made available on CRAN for a
        builtin configuration to be usable by `{piecepackr}`.

## <a name="similar">Related Software</a>

### R packages

-   [bdftools](https://github.com/coolbutuseless/bdftools) provides some
    tools for reading, manipulating, and outputting BDF bitmap fonts. In
    particular provides much richer graphical output capabilities than
    `{bittermelon}` will likely support including a specialized
    `{ggplot2}` geom.
-   [fontr](https://github.com/yixuan/fontr) allows one to extract
    character glyphs from a specific font (which itself may not be a
    bitmap font) as a bitmap.
-   [raster](https://rspatial.org/raster/pkg/index.html) has tools for
    manipulating “raster” objects with a focus on spatial applications.

### Python

-   [BDF Parser Python library](https://github.com/tomchen/bdfparser)
-   [bdflib](https://gitlab.com/Screwtapello/bdflib)
-   [monobit](https://github.com/robhagemans/monobit) lets one modify
    bitmap fonts and convert between several formats. Embedded within
    `{bittermelon}`.

### Other

-   [Bits’n’Picas](https://github.com/kreativekorp/bitsnpicas)
-   [FontForge](http://fontforge.org/en-US/)

### Fonts

-   [GNU Unifont](https://www.unifoundry.com/unifont/index.html)

-   [Hoard of
    Bitfonts](https://github.com/robhagemans/hoard-of-bitfonts)

-   [bitmap-fonts](https://github.com/Tecate/bitmap-fonts)

-   Embedded in `{bdftools}` package:

    -   [Cozette](https://github.com/slavfox/Cozette)
    -   [Creep2](https://github.com/raymond-w-ko/creep2)
    -   [Spleen](https://github.com/fcambus/spleen)
