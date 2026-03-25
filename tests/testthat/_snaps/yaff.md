# read_yaff() and write_yaff()

    Code
      write_yaff(pixmap_font, tempfile())
    Condition
      Error in `write_yaff()`:
      ! `write_yaff()` only supports <bm_bitmap> glyphs.
      i Use `bm_lapply(font, as_bm_bitmap)` to cast glyphs.

---

    Code
      write_yaff(multi, tempfile())
    Condition
      Error in `write_yaff()`:
      ! `write_yaff()` doesn't support multi-colored glyphs.
      i Use `bm_clamp()` to cast to black-and-white glyphs.

