# bm_font()

    Code
      validate_bm_font(l)
    Condition
      Error in `validate_bm_font()`:
      ! 'x' must be a **named** list (with Unicode code point names)

---

    Code
      validate_bm_font(l)
    Condition
      Error in `validate_bm_font()`:
      ! Some names were not coercible by `Unicode::as_u_char()`

# summary.bm_font()

    Code
      print(s)
    Output
      Bitmap font
      Bitmap class: bm_bitmap 
      Total characters: 2 
      Unicode block coverage (n / named chars in block):
        Basic Latin  2/95

# summary.bm_font() snapshot with empty font

    Code
      print(summary(bm_font()))
    Output
      Bitmap font
      Total characters: 0 

# summary.bm_font() snapshot with fixed 4x6 font

    Code
      print(summary(font))
    Output
      Bitmap font: Fixed Medium 6 
      Bitmap class: bm_bitmap 
      Total characters: 919 
      Unicode block coverage (n / named chars in block):
        Basic Latin                   95/ 95
        Latin-1 Supplement            96/ 96
        Latin Extended-A             128/128
        Latin Extended-B               6/208
        IPA Extensions                 1/ 96
        Spacing Modifier Letters       9/ 80
        Greek and Coptic              76/135
        Cyrillic                      96/256
        Hebrew                        27/ 88
        Latin Extended Additional     22/256
        General Punctuation           28/111
        Superscripts and Subscripts    1/ 42
        Currency Symbols               2/ 33
        Letterlike Symbols             4/ 80
        Number Forms                   4/ 60
        Arrows                         6/112
        Mathematical Operators       138/256
        Miscellaneous Technical        7/256
        Control Pictures               7/ 42
        Box Drawing                  128/128
        Block Elements                22/ 32
        Geometric Shapes               3/ 96
        Miscellaneous Symbols         11/256
        Specials                       1/  5

