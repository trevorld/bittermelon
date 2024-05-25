#' Sprites for twenty food crops
#'
#' `food_crop_sprites()` returns a named list of lists
#' of twenty food crops in six various stages as [bm_pixmap()] objects.
#'
#' @examples
#' crops <- food_crop_sprites()
#' names(crops)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(crops$corn[[6L]], compress = "v")
#' }
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(crops$orange[[5L]], compress = "v")
#' }
#' @return A named list of lists of six [bm_pixmap()] objects (one through five stages of growth plus a portrait for each crop).  The named list has the following twenty crop names:
#'   * "avocado"
#'   * "cassava"
#'   * "coffee" 
#'   * "corn"
#'   * "cucumber"
#'   * "eggplant"
#'   * "grapes"
#'   * "lemon"
#'   * "melon"
#'   * "orange"
#'   * "pineapple"
#'   * "potato"
#'   * "rice"
#'   * "rose"
#'   * "strawberry"
#'   * "sunflower"
#'   * "tomato"
#'   * "tulip"
#'   * "turnip"
#'   * "wheat"
#' @export
food_crop_sprites <- function() {
    f <- system.file("sprites/FarmingCrops16x16/Crop_Spritesheet.png",
                     package = "bittermelon")
    img <- png::readPNG(f)
    pm <- as_bm_pixmap(img)
    crops <- c("avocado", "cassava", "coffee", "corn", "cucumber",
              "eggplant", "grapes", "lemon", "melon", "orange",
              "pineapple", "potato", "rice", "rose", "strawberry",
              "sunflower", "tomato", "tulip", "turnip", "wheat")
    l <- list()
    for (crop in crops) {
        if (crop %in% c("cucumber", 
                        "turnip",
                        "tomato",
                        "eggplant",
                        "pineapple",
                        "wheat",
                        "strawberry",
                        "potato",
                        "orange",
                        "corn"))
            x0 <- 1L
        else
            x0 <- 1L + 6L * 16L
        y0 <- switch(crop,
                     avocado = 1L + 16L,
                     cassava = 1L + 3L * 16L,
                     coffee = 1L + 2L * 16L,
                     corn = 1L,
                     cucumber = 1L + 8L * 16L,
                     eggplant = 1L + 6L * 16L,
                     grapes = 1L + 4L * 16L,
                     lemon = 1L + 6L * 16L,
                     melon = 1L + 7L * 16L,
                     orange = 1L + 16L,
                     pineapple = 1L + 5L * 16L,
                     potato = 1L + 2L * 16L,
                     rice = 1L + 5L * 16L,
                     rose = 1L + 9L * 16L,
                     strawberry = 1L + 3L * 16L,
                     sunflower = 1L,
                     tomato = 1L + 7L * 16L,
                     tulip = 1L + 8L * 16L,
                     turnip = 1L + 9L * 16L,
                     wheat = 1L + 4L * 16L
        )
        l[[crop]] <- list()
        for (stage in 1:6) {
            l[[crop]][[stage]] <- pm[seq.int(y0, length.out = 16L),
                                     seq.int(x0 + (6L - stage) * 16L, length.out = 16L)]
        }
    }
    l
}
