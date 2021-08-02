bittermelon 0.1.0
=================

* `bm_bitmap()` creates a S3 object representing bitmaps (#1). 
   Intended to represent bitmap font glyphs but can be used
   to represent more complicated bitmaps.
   Non-binary bitmaps are allowed 
   (but we are unlikely to ever support exporting color bitmap fonts).
   It supports the following methods:

  * `as.character.bm_bitmap()`
  * `as.matrix.bm_bitmap()`
  * `as.raster.bm_bitmap()` (#3)
  * `cbind.bm_bitmap()`
  * `plot.bm_bitmap()` (#4)
  * `print.bm_bitmap()` (#2)
  * `rbind.bm_bitmap()`

  * `as_bm_bitmap()` is a S3 method that coerces objects to `bm_bitmap()` objects
  
    * `as_bm_bitmap.default()`
    * `as_bm_bitmap.matrix()`

  * `is_bm_bitmap()`  returns `TRUE` for `bm_bitmap()` objects (or subclasses)
    and `FALSE` for all other objects.

* `bm_list()` creates a S3 object representing a list of `bm_bitmap()` objects.

  * `as_bm_list()` is a S3 method that coerces objects to `bm_list()` objects

    * `as_bm_list.character()` (#28)
    * `as_bm_list.default()` 
    * `as_bm_list.list()` 

  * `is_bm_list()`  returns `TRUE` for `bm_list()` objects (or subclasses)
    and `FALSE` for all other objects.

* `bm_font()` creates a S3 object representing bitmap fonts (#5).

  * `as_bm_font()` is a S3 method that coerces objects to `bm_font()` objects

    * `as_bm_font.default()`
    * `as_bm_font.list()` 

  * `is_bm_font()` returns `TRUE` for `bm_font()` objects (or subclasses)
    and `FALSE` for all other objects.

* The following functions can modify `bm_bitmap()` objects 
  as well as all the bitmaps in `bm_list()` objects (including `bm_font()`):

  * `bm_clamp()` clamps integer values between a lower and upper value.
    By default coerces the bitmap to binary values.
  * `bm_extend()` extends the bitmap by a specified value in specified directions (#11).
  * `bm_resize()` resized the bitmap to a desired width and/or height.
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

  * `bm_lapply()` applies a function to each element in a `bm_list()` or `bm_font()` object.
    It returns another `bm_list()` or `bm_font()` object with the same metadata as
    the original object.
  * `bm_widths()` and `bm_heights()` calculates the widths and heights of the bitmaps
    in `bm_list()` or `bm_font()` objects.
  * `bm_padding_lengths()` computes the padding lengths of a
    target value for the top, right, bottom, and left sides of the bitmap.
  * `hex2ucp()`, `int2ucp()`, `name2ucp()` and  `str2ucp()`
  return Unicode code points as character vectors.
