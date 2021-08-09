# bittermelon <img src="man/figures/logo.png" align="right" width="200px" alt="bittermelon hex sticker">

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/bittermelon)](https://cran.r-project.org/package=bittermelon)
[![R-CMD-check](https://github.com/trevorld/bittermelon/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/bittermelon/actions)
[![Coverage Status](https://img.shields.io/codecov/c/github/trevorld/bittermelon.svg)](https://codecov.io/github/trevorld/bittermelon?branch=main)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Builtin Fonts](#fonts)
* [GNU Unifont via {hexfont}](#hexfont)
* [Future Goals](#future)
* [Related Software](#similar)

## <a name="overview">Overview</a>

`{bittermelon}` provides functions for creating and modifying bitmaps with special emphasis on bitmap fonts and their glyphs.  It provides native read/write support for the 'hex' and 'yaff' bitmap font formats and if 'Python' is installed can also read/write several more bitmap font formats using an embedded version of [monobit](https://github.com/robhagemans/monobit).  It features [over a dozen functions](https://trevorldavis.com/R/bittermelon/dev/reference/index.html#section-modify-bitmaps) that can modify individual bitmaps or every bitmap within a "bitmap list" or "bitmap font".  `{bittermelon}` can also pretty print bitmaps to the terminal and has a basic plot method.

**This project is currently a 'Work-in-Progress'.  Features are being implemented and the API is not yet stable.**  Check out the awesome [{bdftools}](https://github.com/coolbutuseless/bdftools) package if you currently need bitmap font support in R.  `{bdftools}` will probably always provide cooler bitmap font graphical output support than `{bittermelon}`.  The primary goal of `{bittermelon}` is to make it easier for me to create new bitmap fonts using R.

## <a name="installation">Installation</a>


```r
remotes::install_github("trevorld/bittermelon")
```

The functions `read_monobit()` and `write_monobit()` that use the embedded version of [monobit](https://github.com/robhagemans/monobit) require that Python is available on the system.  A couple of the bitmap font output formats supported by `write_monobit()` also require that the "Pillow" or "reportlab" Python packages are installed (installable via `pip3`).

## <a name="examples">Examples</a>




```r
library("bittermelon")
```

```

Attaching package: 'bittermelon'
```

```
The following object is masked from 'package:base':

    which
```

```r
font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
bml <- as_bm_list("RSTATS", font = font)
# With vertical compression
bm <- bml |> bm_call(cbind) |> bm_compress("vertical")
print(bm)
```

```{.short}
                                                
██▀▀▀█▄ ▄█▀▀▀▀▀ ▀▀▀██▀▀▀▄█▀▀▀█▄ ▀▀▀██▀▀▀▄█▀▀▀▀▀ 
██   ██ ██         ██   ██   ██    ██   ██      
██▀▀▀█▄  ▀▀▀▀█▄    ██   ██▀▀▀██    ██    ▀▀▀▀█▄ 
██   ██      ██    ██   ██   ██    ██        ██ 
██   ██ ▄▄▄▄▄█▀    ██   ██   ██    ██   ▄▄▄▄▄█▀ 
                                                
                                                
```

```r
# Upside down with ASCII characters
bm <- bml |> 
    bm_flip("both") |> 
    bm_call(cbind, direction = "RTL")
print(bm, px = px_ascii)
```

```{.short}
------------------------------------------------
------------------------------------------------
------------------------------------------------
------------------------------------------------
--@@@@@@---@@----@@---@@---@@-----@@@@@@-@@---@@
-@@--------@@----@@---@@---@@----@@------@@---@@
-@@--------@@----@@---@@---@@----@@------@@---@@
-@@--------@@----@@---@@---@@----@@------@@---@@
-@@--------@@----@@---@@---@@----@@------@@---@@
--@@@@@----@@----@@@@@@@---@@-----@@@@@---@@@@@@
------@@---@@----@@---@@---@@---------@@-@@---@@
------@@---@@----@@---@@---@@---------@@-@@---@@
------@@---@@----@@---@@---@@---------@@-@@---@@
-@@@@@@-@@@@@@@@--@@@@@-@@@@@@@@-@@@@@@---@@@@@@
------------------------------------------------
------------------------------------------------
```

```r
# With a shadow effect and borders
bm <- bml |> 
    bm_pad(sides = 2L) |>
    bm_shadow() |>
    bm_extend(sides = c(2L, 1L), value = 3L) |>
    bm_call(cbind) |> 
    bm_pad(sides = 2L, value = 3L)
print(bm)
```

```{.short}
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
```

```r
# Can also print colored terminal output via suggested package {crayon}
if (crayon::has_color())
    print(bm, px = " ", bg = c("white", "black", "grey", "red"))
```

```r
plot(bm, col = c("white", "darkblue", "lightblue", "black"))
```

![](man/figures/README-plot-1.png)

## <a name="fonts">Builtin Fonts</a>

`{bittermelon}` has a builtin versions of the 8x16 [Spleen](https://github.com/fcambus/spleen) font as well as 4x6 and 6x13 [Fixed](https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html) fonts.


```r
spleen_8x16 <- read_hex(system.file("fonts/spleen/spleen-8x16.hex.gz",
                                    package = "bittermelon"))
fixed_4x6 <- read_yaff(system.file("fonts/fixed/4x6.yaff.gz", 
                                   package = "bittermelon"))
fixed_6x13 <- read_yaff(system.file("fonts/fixed/6x13.yaff.gz", 
                                    package = "bittermelon"))
as_bm_list("RSTATS", font = spleen_8x16) |> 
    bm_call(cbind) |> 
    bm_compress("v")
```

```{.short}
                                                
██▀▀▀█▄ ▄█▀▀▀▀▀ ▀▀▀██▀▀▀▄█▀▀▀█▄ ▀▀▀██▀▀▀▄█▀▀▀▀▀ 
██   ██ ██         ██   ██   ██    ██   ██      
██▀▀▀█▄  ▀▀▀▀█▄    ██   ██▀▀▀██    ██    ▀▀▀▀█▄ 
██   ██      ██    ██   ██   ██    ██        ██ 
██   ██ ▄▄▄▄▄█▀    ██   ██   ██    ██   ▄▄▄▄▄█▀ 
                                                
                                                
```

```r
as_bm_list("RSTATS", font = fixed_4x6) |>
    bm_call(cbind) |> 
    bm_compress("v")
```

```{.short}
█▀▄ ▄▀▀ ▀█▀ ▄▀▄ ▀█▀ ▄▀▀ 
█▀▄  ▀▄  █  █▀█  █   ▀▄ 
▀ ▀ ▀▀   ▀  ▀ ▀  ▀  ▀▀  
```

```r
as_bm_list("RSTATS", font = fixed_6x13) |>
    bm_call(cbind) |> 
    bm_compress("v")
```

```{.short}
                                    
█▀▀▀▄ ▄▀▀▀▄ ▀▀█▀▀  ▄▀▄  ▀▀█▀▀ ▄▀▀▀▄ 
█   █ █       █   █   █   █   █     
█▀█▀   ▀▀▀▄   █   █▄▄▄█   █    ▀▀▀▄ 
█  ▀▄ ▄   █   █   █   █   █   ▄   █ 
▀   ▀  ▀▀▀    ▀   ▀   ▀   ▀    ▀▀▀  
                                    
```

## <a name="hexfont">GNU Unifont via {hexfont}</a>

The [{hexfont}](https://github.com/trevorld/hexfont) package includes a helper function `unifont()` which loads several GNU Unifont hex fonts as a single `{bittermelon}` `bm_font()` object.  [GNU Unifont](http://unifoundry.com/unifont/index.html) is a monoscale bitmap font (8x16 and 16x16 glyphs) that pretty much covers all of the official Unicode glyphs plus several of the artificial scripts in the [(Under-)ConScript Unicode Registry](http://www.kreativekorp.com/ucsur/).



```r
library("hexfont") # remotes::install_github("trevorld/hexfont")
# Note Unifont is a **big** font
system.time(font <- unifont())
```

```{.short}
   user  system elapsed 
 58.487   0.107  58.603 
```

```r
length(font)
```

```{.short}
[1] 77418
```

```r
object.size(font)
```

```{.short}
121681880 bytes
```

```r
# Mandarin Chinese
as_bm_list("Ｒ很棒！", font = font) |>
    bm_call(cbind) |> 
    bm_compress("v")
```

```{.short}
                    █ ▄▄▄▄▄▄▄      █      █                     
                  ▄▀  █     █      █  ▀▀▀▀█▀▀▀▀                 
  ██▀▀▀▀▀▀▀▀▄▄   ▀  █ █▀▀▀▀▀█   ▀▀▀█▀▀ ▀▀█▀▀▀▀         ██       
  ██        ██    ▄█  █▄▄▄▄▄█     ██▄ ▀▀█▀▀▀█▀▀        ██       
  ██▀▀▀▀██▀▀    ▄▀ █  █  █  ▄▀   █ █ ▀▄▀  █  ▀▄        ██       
  ██      ██       █  █   █▀    ▀  █    ▀▀█▀▀          ▀▀       
  ██        ██     █  █ ▄  ▀▄      █  ▀▀▀▀█▀▀▀▀        ██       
                   █  █▀     ▀▀    █      █                     
```

```r
# Emoji
as_bm_list("🐭🐲🐵", font = font) |>
    bm_call(cbind) |> 
    bm_compress("v")
```

```{.short}
  ▄▄       ▄▄           ▄▄▄            ▄▄       
▄▀  ▀▄▄▄▄▄▀  ▀▄       ▄█▀           ▄█▀██▀█▄    
█    ▀   ▀    █      ▄████       ▄▀█ ▄▄  ▄▄ █▀▄ 
▄█   ▀   ▀   █▄   ▄▄██▄█████     ▀▄█ ▀▀  ▀▀ █▄▀ 
▄█▀    ▄    ▀█▄ ▄█▄███████████     ▄▀      ▀▄   
  ▀▄  ▀▀▀  ▄▀   ▀▀▀▀▀███▀▀█████    █ ▀▄▄▄▄▀ █   
    ▀▀▄▄▄▀▀        ▄███   ████▀     ▀▀▄▄▄▄▀▀    
                  ▀▀▀     ▀▀▀                   
```

## <a name="future">Future Goals</a>

* Generate enough bitmap manipulation capabilities in order to create a specialized monoscale font to support a [boardgame diagram use case no existing Unicode font seems to support well](https://trevorldavis.com/piecepackr/unicode-piecepack-diagrams.html#piecepack-font-wishlist).

  * A 16x16 "hex" font could be a good initial prototype target.

* Enable people to render such board game diagrams using the font in R (and perhaps XeLaTeX or HTML) even if the user doesn't have the above font installed.  Ideally the rendering support must support Unicode "combining" characters, ideally `{crayon}` coloring, and ideally "compression" using "Block characters".
* Generate ability to efficiently store and use a few bitmap icons in `{piecepackr}` to provide a "configuration" users could use to generate certain game icons if they don't have the right fonts installed on their system for more "natural" font based approaches.

  * As an alternative don't want to bundle a bunch of large images with `{piecepackr}`
  * In particular "sans" lacks Chess symbols and certain Piecepack symbols.  Additionally "Dejavu Sans" lacks good Xiangqi and Shogi symbols.  Certain games (we might never support) such as Arimaa and Hive could be straightforwardly implemented with various Emoji animal glyphs.
  * `{bittermelon}` must eventually be made available on CRAN for a builtin configuration to be usable by `{piecepackr}`.

## <a name="similar">Related Software</a>

### R packages

* [bdftools](https://github.com/coolbutuseless/bdftools) provides some tools for reading, manipulating, and outputting BDF bitmap fonts.  In particular provides much richer graphical output capabilities than `{bittermelon}` will likely support including a specialized `{ggplot2}` geom. 
* [fontr](https://github.com/yixuan/fontr) allows one to extract character glyphs from a specific font (which itself may not be a bitmap font) as a bitmap.
* [raster](https://rspatial.org/raster/pkg/index.html) has tools for manipulating "raster" objects with a focus on spatial applications.

### Python

* [BDF Parser Python library](https://github.com/tomchen/bdfparser)
* [bdflib](https://gitlab.com/Screwtapello/bdflib)
* [monobit](https://github.com/robhagemans/monobit) lets one modify bitmap fonts and convert between several formats.  Embedded within `{bittermelon}`.

### Other

* [Bits'n'Picas](https://github.com/kreativekorp/bitsnpicas)
* [FontForge](http://fontforge.org/en-US/)

### Fonts

* [Fixed](https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html) aka "Unicode fonts and tools for X11"
* Embedded in `{hexfont}` package:

  * [GNU Unifont](https://www.unifoundry.com/unifont/index.html)

* [Hoard of Bitfonts](https://github.com/robhagemans/hoard-of-bitfonts)
* [bitmap-fonts](https://github.com/Tecate/bitmap-fonts)
* Embedded in `{bdftools}` package:

  * [Cozette](https://github.com/slavfox/Cozette)
  * [Creep2](https://github.com/raymond-w-ko/creep2) 
  * [Spleen](https://github.com/fcambus/spleen)
