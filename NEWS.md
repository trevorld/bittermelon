bittermelon 0.0.0
=================

* `bm_glyph()` creates creates an S3 object representing (monochrome) bitmap (font) glyphs (#1). 
   It supports the following methods:

  * `as.character.bm_glyph()`
  * `as.matrix.bm_glyph()`
  * `print.bm_glyph()` (#2)

* `as_bm_glyph()` is a S3 method that coerces objects to `bm_glyph()` objects

  * `as_bm_glyph.default()`
  * `as_bm_glyph.matrix()`

* `is_bm_glyph()`  returns `TRUE` for `bm_glyph` objects (or subclasses)
  and `FALSE` for all other objects.
