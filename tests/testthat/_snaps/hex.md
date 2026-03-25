# read_hex() and write_hex()

    Code
      write_hex(pixmap_font, tempfile())
    Condition
      Error in `write_hex()`:
      ! `write_hex()` only supports <bm_bitmap> glyphs.
      i Use `bm_lapply(font, as_bm_bitmap)` to cast glyphs.

---

    Code
      write_hex(multi, tempfile())
    Condition
      Error in `write_hex()`:
      ! `write_hex()` doesn't support multi-colored glyphs.
      i Use `bm_clamp()` to cast to black-and-white glyphs.

