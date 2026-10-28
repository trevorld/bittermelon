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
        Basic Latin                   95/<total>
        Latin-1 Supplement            96/<total>
        Latin Extended-A             128/<total>
        Latin Extended-B               6/<total>
        IPA Extensions                 1/<total>
        Spacing Modifier Letters       9/<total>
        Greek and Coptic              76/<total>
        Cyrillic                      96/<total>
        Hebrew                        27/<total>
        Latin Extended Additional     22/<total>
        General Punctuation           28/<total>
        Superscripts and Subscripts    1/<total>
        Currency Symbols               2/<total>
        Letterlike Symbols             4/<total>
        Number Forms                   4/<total>
        Arrows                         6/<total>
        Mathematical Operators       138/<total>
        Miscellaneous Technical        7/<total>
        Control Pictures               7/<total>
        Box Drawing                  128/<total>
        Block Elements                22/<total>
        Geometric Shapes               3/<total>
        Miscellaneous Symbols         11/<total>
        Specials                       1/<total>

