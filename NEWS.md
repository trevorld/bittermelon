bittermelon 0.1.0
=================

* `bm_glyph()` creates a S3 object representing bitmap (font) glyphs (#1). 
   It supports the following methods:

  * `as.character.bm_glyph()`
  * `as.matrix.bm_glyph()`
  * `as.raster.bm_glyph()` (#3)
  * `plot.bm_glyph()` (#4)
  * `print.bm_glyph()` (#2)

  * `as_bm_glyph()` is a S3 method that coerces objects to `bm_glyph()` objects
  
    * `as_bm_glyph.default()`
    * `as_bm_glyph.matrix()`

  * `is_bm_glyph()`  returns `TRUE` for `bm_glyph()` objects (or subclasses)
    and `FALSE` for all other objects.

* `bm_list()` creates a S3 object representing a list of `bm_glyph()` objects.

  * `as_bm_list()` is a S3 method that coerces objects to `bm_list()` objects

    * `as_bm_list.default()` 
    * `as_bm_list.list()` 

  * `is_bm_list()`  returns `TRUE` for `bm_list()` objects (or subclasses)
    and `FALSE` for all other objects.

  * `lapply_bm_list()` applies a function to each element in a `bm_list()` object.

* `bm_font()` creates a S3 object representing bitmap fonts (#5).

  * `as_bm_font()` is a S3 method that coerces objects to `bm_font()` objects

    * `as_bm_font.default()`
    * `as_bm_font.list()` 

  * `is_bm_font()` returns `TRUE` for `bm_font()` objects (or subclasses)
    and `FALSE` for all other objects.

* The following functions can modify `bm_glyph()` objects 
  as well as all the glyphs in `bm_list()` objects (including `bm_font()`):

  * `bm_clamp()` clamps integer values between a lower and upper value.
    By default coerces the glyph to binary values.

* Support for reading and writing bitmap fonts

    * `read_hex()` reads in a "hex" font as a `bm_font()` object (#8)
    * `write_hex()` writes out a "hex" font of a `bm_font()` object (#9)

* `code_point()`, `code_point_from_char()`, and `code_point_from_name()` 
  return Unicode code points as character vectors.
