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
