bittermelon 2.0.2
=================

Breaking changes
----------------

* We no longer embed an increasingly outdated version of [monobit](https://github.com/robhagemans/monobit).

  - `monobit` has now grown too large to embed within `{bittermelon}` and stay within CRAN package size limits.
  - Users interested in reading/writing bitmap fonts formats other than `.hex` and `.yaff` files
    will need to manually install their own version of `monobit` using `pip3 install monobit`.
  - The interpretation of the argument `monobit_path` in `read_monobit()` and `write_monobit()`
    and the global option `bittermelon.monobit_path` has been changed and now means the path
    to pass to `base::Sys.which()` (if the new default name `"monobit-convert"` is not good enough).
  - Any previously installed versions of `monobit` to `rappdirs::site_config_dir("bittermelon", "monobit")` or `rappdirs::user_config_dir("bittermelon", "monobit")` will now be ignored.
    Please consider deleting them or update the `bittermelon.monobit_path` option to point to them.
  - The function `update_monobit()` has been removed.

* The default `col` argument to `as.raster.bm_bitmap()` and `plot.bm_bitmap()` is now
  `getOption("bittermelon.col", col_bitmap)` where the newly exported
  `col_bitmap = c("transparent", "black", "grey50", "grey25")`.
  In particular by default `0L` values will now be cast to "transparent" instead of "grey80".
  To get the old behaviour set `options("bittermelon.col" = c("grey80", "black", "grey40"))`.

* We no longer export a generic S3 `which()` method which by default calls `base::which()`
  and remove the special `which.bm_bitmap()` method that first cast to a logical.
  Instead now use `which(as.logical(x))` for `bm_bitmap()` objects.

* Many functions were upgraded to S3 generics that support more bitmap objects.
  The first argument of these functions are now usually `x` instead of `bm_object`.
  If specifying the first argument by name instead of positionally you'll now
  need to use `x` instead.

* `bm_distort()` is now a wrapper around `magick::image_resize()`.
  `x`, `width`, and `height` are now the only supported *positional* arguments
  and the `interpolate`, `vp`, and `png_device` arguments are no longer supported.
  It now requires the suggested package `{magick}`.

* The default for `height` in `bm_expand()` is now the value of `width` i.e.
  `bm_expand(x, 2L)` will now double the size of `x` in both directions.
  To get the previous behaviour you may now need to explicitly set `height = 1L`.

New features
------------

* `bm_pixmap()` creates a S3 object representing raster image pixmaps (#60).
  Intended to represent pixel art / sprites but can be used
  to represent more complicated raster images.
  It supports the following S3 methods:

  * `as.matrix.bm_pixmap()` and `as.array.bm_pixmap()`
  * `as.raster.bm_pixmap()` and `plot.bm_pixmap()`
  * `cbind.bm_pixmap()` and `rbind.bm_pixmap()`
  * `format.bm_pixmap()` and `print.bm_pixmap()`
  * `as_bm_pixmap()` is a S3 method that coerces objects to `bm_pixmap()` objects

    - `as_bm_pixmap.array()` (e.g. from `{png}`, `{jpeg}`, and `{webp}`)
    - `as_bm_pixmap.bm_bitmap()`
    - `as_bm_pixmap.default()`
    - `as_bm_pixmap.glyph_bitmap()` (from `{fontr}`)
    - `as_bm_pixmap.grob()`
    - `as_bm_pixmap.magick-image()` (from `{magick}`)
    - `as_bm_pixmap.matrix()`
    - `as_bm_pixmap.maze()` (from `{mazing}`)
    - `as_bm_pixmap.nativeRaster()`
    - `as_bm_pixmap.pattern_square()` (from `{gridpattern}`)
    - `as_bm_pixmap.pattern_weave()` (from `{gridpattern}`)
    - `as_bm_pixmap.pixeltrix()` (from `{pixeltrix}`)
    - `as_bm_pixmap.pixmapGrey()` (from `{pixmap}`)
    - `as_bm_pixmap.pixmapIndexed()` (from `{pixmap}`)
    - `as_bm_pixmap.pixmapRGB()` (from `{pixmap}`)
    - `as_bm_pixmap.raster()`

  * `is_bm_pixmap()`  returns `TRUE` for `bm_pixmap()` objects (or subclasses)
    and `FALSE` for all other objects.
  * Some of the "Ops" group generic operators such as `==` and `!=`.

* The following functions are now S3 generics that have methods that support (at least) `bm_bitmap()` / `bm_pixmap()`, `bm_font()` / `bm_list()`, "magick-image", and "raster" / "nativeRaster" objects:

  * `bm_bold()`
  * `bm_clamp()`
  * `bm_compress()`
  * `bm_distort()`
  * `bm_expand()`
  * `bm_extend()`
  * `bm_flip()`
  * `bm_glow()`
  * `bm_heights()`
  * `bm_mask()`
  * `bm_outline()`
  * `bm_overlay()`
  * `bm_pad()`
  * `bm_padding_lengths()`
  * `bm_resize()`
  * `bm_rotate()`
  * `bm_shadow()`
  * `bm_shift()`
  * `bm_trim()`
  * `bm_widths()`

* New bitmap/pixmap manipulation function:

  * `bm_downscale()`
  * `bm_gray()` with alias `bm_grey()`
  * `bm_invert()`
  * `bm_replace()`

* Other new functions:

  * `bm_print()` and `bm_format()` can be used to pretty print bitmap objects to the terminal.
  * `farming_crops_16x16()` returns a named list of lists
    of twenty farming crops in five stages of growth plus a portrait as `bm_pixmap()` objects.
  * `bm_options()` returns a list of (current or default) `bittermelon` options values.
  * `px_auto()` determines which character vector to use for "pixels" based on
    whether `cli::is_utf8_output()` is `TRUE` or not.

* New `as_bm_bitmap()` class methods:

  + `as_bm_bitmap.array()` (e.g. from `{png}`, `{jpeg}`, and `{webp}`)
  + `as_bm_bitmap.bm_pixmap()`
  + `as_bm_pixmap.glyph_bitmap()` (from `{fontr}`)
  + `as_bm_bitmap.magick-image()` (from `{magick}`)
  + `as_bm_bitmap.maze()` coerces (from `{mazing}`)
  + `as_bm_bitmap.nativeRaster()`
  + `as_bm_bitmap.pattern_square()` (from `{gridpattern}`)
  + `as_bm_bitmap.pattern_weave()` (from `{gridpattern}`)
  + `as_bm_bitmap.pixeltrix()` (from `{pixeltrix}`)
  + `as_bm_bitmap.pixmapGrey()` (from `{pixmap}`)
  + `as_bm_bitmap.pixmapIndexed()` (from `{pixmap}`)
  + `as_bm_bitmap.pixmapRGB()` (from `{pixmap}`)
  + `as_bm_bitmap.raster()`

* New `"bm_bitmap"` class S3 generics features:

  * `format.bm_bitmap()` and `print.bm_bitmap()` gain a `downscale` argument to downscale the image
    to `getOption("width")` (if necessary to fit in terminal).
  * `as.raster.bm_bitmap()` now has a `native` argument to cast to "nativeRaster" objects.
  * `as.matrix.bm_bitmap()` now has a `first_row_is_top` argument to flip the rows
    so that the first row represents the top of the bitmap instead of the bottom.
  * New `as.array.bm_bitmap()` function for writing bitmaps with `png::writePNG()` and friends.

* New color utilities:

  * `col2hex()` standardize color strings into a unique hex color string.
  * `col2int()` and `int2col()` convert back and forth to (native) color integers.

* Following `bm_list()` upgrades (#69):

  * Can now contain `bm_bitmap()`, `bm_pixmap()`, "magick-image", "nativeRaster", or "raster" bitmaps.
  * `as_bm_list.list()` now supports `FUN` for a function which can be applied to each element of the list
    such as `identity()`, `as_bm_bitmap()`, and `as_bm_pixmap()`.

* New package options:

  * The `bittermelon.downscale` option can be used to change the default for
    the `downscale` argument in `format.bm_bitmap()`, `print.bm_bitmap()`,
    `format.bm_pixmap()`, and `print.bm_pixmap()`.

Bug fixes and minor improvements
--------------------------------

* The `bg` and `fg` arguments of `format.bm_bitmap()` and `print.bm_bitmap()`
  now accept lists of `{cli}` ANSI style functions in addition to color strings.
* `print.bm_bitmap()` no longer silently ignores its `compress` argument.
* The default value of the option `bittermelon.px` is now `px_auto()`.
  This means if `cli::is_utf8_output()` is `FALSE` we now default to `px_ascii` instead of `px_unicode`.
* Fixes bugs in `write_monobit()`, `write_yaff()`, and the unit tests
  if any of the options `bittermelon.fg`, `bittermelon.bg`, or `bittermelon.compress`
  are set away from their defaults.
* `read_yaff()` now correctly handles octal codepoint and (single) character labels.
* `write_yaff()` now encloses "tag" labels with double quotes.
* `hex2ucp()` now also calls `base::toupper()` on the input value.
* `bm_bitmap()` objects now also have the class `"bm_matrix"` (as does the new `bm_pixmap()` objects).
* `format.bm_bitmap(x)` no longer throws an error when `x` has zero columns (#68).
* `bm_flip()` now has a `value` argument to set the background padding value to use
  if `in_place` is `TRUE`.

bittermelon 1.1.2
=================

Bug fixes and minor improvements
--------------------------------

* Updates `is_combining_character()` so it will continue to work after a breaking change 
  in `Unicode::u_char_property()`'s behavior introduced in `{Unicode}` v15.1.0-1 (#55).

bittermelon 1.1.1
=================

New features
------------

* `read_hex()` has new argument `ucp`.  
  Character vector of Unicode Code Points: glyphs not in this vector won't be read in.
  If `NULL` (default) read every glyph in the font (#52).

Bug fixes and minor improvements
--------------------------------

* `block2ucp()` and `str2ucp()` are now vectorized in their first argument.

bittermelon 1.0.0
=================

This update has no user-facing changes (at the request of CRAN we re-built our package man pages with an updated version of the `{roxygen2}` package to avoid a deprecated html image alignment warning).

bittermelon 0.2.1
=================

New features
------------

* New function `bm_compose()` simplifies `bm_list()` object
  by applying combining marks to preceding glpyhs (composing new graphemes) (#42).
* `as_bm_bitmap.character()` has new arguments `compose` and `pua_combining`
  to compose graphemes using combining characters.
* `as_bm_bitmap.character()` direction argument now supports combining
  in combinations of horizontal/vertical directions such as
  `left-to-right, top-to-bottom` (#47).
* `is_combining_character()` has new argument `pua_combining`
  which is character vector of additional Unicode code points
  to be considered "combining" characters (such as those
  in the Private Use Area of a font).  Defaults to `character(0)`.
* We now include the 5x8 Fixed font (in addition to the 4x6 and 6x13 Fixed fonts already included in earlier version).
* `read_monobit()` and `write_monobit()` take new argument `monobit_path` indicating
  the directory path containing `monobit` to use.  
  Default will be to look in `file.path(rappdirs::user_config_dir("bittermelon"), "monobit")`,
  `file.path(rappdirs::site_config_dir("bittermelon"), "monobit")`, and
  `system.file("monobit", package = "bittermelon")` (in that order).
  New package option `bittermelon.monobit_path` can be used to set a new default. (#48)
* New function `update_monobit()` which downloads the most up to date (upstream) version of 
  [monobit](https://github.com/robhagemans/monobit).
  Although we continue to embed an older, more compact version of `monobit` in this package
  the newest versions of `monobit` are too large to embed within this package and
  must be downloaded separately.

Bug fixes and minor improvements
--------------------------------

* Updates the embedded version of [monobit](https://github.com/robhagemans/monobit).
  In particular `monobit` should now better handle Amiga, [AngelCode BMFont](http://www.angelcode.com/products/bmfont/), X11/Adobe BDF, and C/C++ source code bitmap fonts and be able to natively read/write 
  bzip2, gzip, or lzma compressed fonts.
* Fixes bug in `is_combining_character()`.  
  Now "combining enclosing" characters are correctly classified as combining characters.
* Fixes bug in `bm_expand()` for bitmaps with zero columns/rows.

bittermelon 0.1.3
=================

* `bm_bitmap()` creates a S3 object representing bitmaps (#1). 
  Intended to represent bitmap font glyphs but can be used
  to represent more complicated bitmaps.
  Non-binary bitmaps are allowed 
  (but we are unlikely to ever support exporting color bitmap fonts).
  It supports the following S3 methods:

  * `[.bm_bitmap()` and `[<-.bm_bitmap()` (#38)
  * `as.matrix.bm_bitmap()`
  * `as.raster.bm_bitmap()` (#3) and `plot.bm_bitmap()` (#4)
  * `cbind.bm_bitmap()` and `rbind.bm_bitmap()`
  * `format.bm_bitmap()` and `print.bm_bitmap()` (#2)
  * `which.bm_bitmap()` (with `which()` redefined as a S3 generic that defaults to `base::which()`)
  * `as_bm_bitmap()` is a S3 method that coerces objects to `bm_bitmap()` objects
  
    * `as_bm_bitmap.default()`
    * `as_bm_bitmap.grob()` (#10)
    * `as_bm_bitmap.matrix()`

  * `is_bm_bitmap()`  returns `TRUE` for `bm_bitmap()` objects (or subclasses)
    and `FALSE` for all other objects.

* `bm_list()` creates a S3 object representing a list of `bm_bitmap()` objects.
  It supports the following S3 methods:

    * The "min()", "max()", and "range()" functions from the "Summary" group generic methods
    * `as.list.bm_list()`
    * "slicing" with `[]` returns `bm_list()` objects.

  * `as_bm_list()` is a S3 method that coerces objects to `bm_list()` objects

    * `as_bm_list.character()` (#28)
    * `as_bm_list.default()` 
    * `as_bm_list.list()` 

  * `is_bm_list()`  returns `TRUE` for `bm_list()` objects (or subclasses)
    and `FALSE` for all other objects.

* `bm_font()` creates a S3 object representing bitmap fonts (#5).
  it is a subclass of the `bm_list()` class.

  * `as_bm_font()` is a S3 method that coerces objects to `bm_font()` objects

    * `as_bm_font.default()`
    * `as_bm_font.list()` 

  * `is_bm_font()` returns `TRUE` for `bm_font()` objects (or subclasses)
    and `FALSE` for all other objects.

* The following functions can modify `bm_bitmap()` objects 
  as well as all the bitmaps in `bm_list()` objects (including `bm_font()`):

  * All the "Ops" group generic operators such as `!`.
  * `bm_bold()` creates a basic "bold" effect (#16)
  * `bm_clamp()` clamps integer values between a lower and upper value.
     By default coerces the bitmap to binary values.
  * `bm_compress()` shrinks bitmaps by a factor of two using a "block elements" scheme (#31).
  * `bm_distort()` resizes bitmaps via distortion.
  * `bm_expand()` expands bitmap(s) by repeating each row and/or column (#19).
  * `bm_extend()` extends the bitmap by a specified value in specified directions (#11).
  * `bm_flip()` flips (reflects) bitmaps (or just their glyphs in place) 
    vertically, horizontally, or both.
  * `bm_glow()` adds a basic "glow" effect (#17)
  * `bm_mask()` uses a `mask` bitmap to set certain pixels in a `base` bitmap to zero (#21).
  * `bm_outline()` computes a "outline" bitmap from a bitmap.
  * `bm_overlay()` merges bitmaps by overlaying one over another (#18).
  * `bm_pad()` adjusts bitmap padding lengths (#40).
  * `bm_resize()` resizes the bitmap to a desired width and/or height via extending/trimming (#34).
  * `bm_rotate()` losslessly rotates bitmaps 0, 90, 180, or 270 degrees (#15).
  * `bm_shadow()` adds a "shadow effect" (#17)
  * `bm_shift()` shifts elements within a bitmap in a specified direction
     while preserving original width and height (#13).
  * `bm_trim()` trims the bitmap in specified directions (#12).

* Support for reading and writing bitmap fonts

    * `read_hex()` reads in a "hex" font as a `bm_font()` object (#8)
    * `write_hex()` writes out a "hex" font of a `bm_font()` object (#9)
    * `read_yaff()` reads in a "yaff" font as a `bm_font()` object (#6)
    * `write_yaff()` writes out a "yaff" font of a `bm_font()` object (#7)
    * `read_monobit()` reads in a bitmap font as a `bm_font()` object using `monobit`.
      Supports multiple bitmap font formats.  Requires Python to be available. (#26)
    * `write_monobit()` writes out a bitmap font of a `bm_font()` object using `monobit`.
      Supports multiple bitmap font formats.  Requires Python to be available. (#27)

* Other utility functions

  * `c()` S3 methods combine bitmap objects.  In particular when using it to combine fonts the later fonts "update" the glyphs in the earlier fonts.
  * `bm_call()` executes a function on bitmap objects.  It places the bitmap object
    as the first argument so it is a bit friendlier to use in pipes than `base::do.call()`
    and allows specifying additional arguments to the function.
  * `bm_edit()` allows editing a `bm_bitmap()` object via text editor
    indicating zeroes with a `.` and ones with a `@` (as in `yaff` font format).
  * `bm_lapply()` applies a function to each element in a `bm_list()` or `bm_font()` object.
    It returns another `bm_list()` or `bm_font()` object with the same metadata as
    the original object.
  * `bm_widths()` and `bm_heights()` calculates the widths and heights of the bitmaps
    in `bm_list()` or `bm_font()` objects.
  * `bm_padding_lengths()` computes the padding lengths of a
    target value for the top, right, bottom, and left sides of the bitmap.
  * `block2ucp()`, `hex2ucp()`, `int2ucp()`, `name2ucp()`, `range2ucp()`, and `str2ucp()`
    return Unicode code points as character vectors.
  * `ucp2label()` returns Unicode code labels as character vectors.
  * `is_combining_character()` returns `TRUE` or `FALSE` if Unicode 
     code point refers to a combining character.
  * `ucp_sort()` sorts Unicode code points
