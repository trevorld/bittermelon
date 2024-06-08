#' Sprites for twenty farming crops
#'
#' `farming_crops_16x16()` returns a named list of [bm_list()] lists
#' of twenty farming crops in five stages of growth plus a portrait as [bm_pixmap()] objects.
#'
#' * Each sprite is sixteen by sixteen pixels large.
#' * [Farming Crops 16x16](https://opengameart.org/content/farming-crops-16x16) was made and dedicated to the public domain by [josehzz](https://opengameart.org/users/josehzz).
#'
#' @examples
#' crops <- farming_crops_16x16()
#' names(crops)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(crops$corn$portrait, compress = "v")
#' }
#'
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(crops$orange$stage5, compress = "v")
#' }
#' @return A named list of [bm_list()] lists of six [bm_pixmap()] objects (one through five stages of growth plus a portrait for each crop).  The named list has the following twenty crop names:
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
farming_crops_16x16 <- function() {
    f <- system.file("sprites/FarmingCrops16x16/Crop_Spritesheet.png",
                     package = "bittermelon")
    img <- png::readPNG(f)
    pm <- as_bm_pixmap(img)
    crops <- c("avocado", "cassava", "coffee", "corn", "cucumber",
              "eggplant", "grapes", "lemon", "melon", "orange",
              "pineapple", "potato", "rice", "rose", "strawberry",
              "sunflower", "tomato", "tulip", "turnip", "wheat")
    l <- list()
    labels <- c(paste0("stage", 1:5), "portrait")
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
        l[[crop]] <- bm_list()
        for (i in 1:6) {
            l[[crop]][[labels[i]]] <- pm[seq.int(y0, length.out = 16L),
                                     seq.int(x0 + (6L - i) * 16L, length.out = 16L)]
        }
    }
    l
}
