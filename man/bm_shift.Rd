% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bm_shift.R
\name{bm_shift}
\alias{bm_shift}
\title{Shift elements within bitmaps}
\usage{
bm_shift(
  bm_object,
  value = 0L,
  top = NULL,
  right = NULL,
  bottom = NULL,
  left = NULL
)
}
\arguments{
\item{bm_object}{Either a \code{\link[=bm_bitmap]{bm_bitmap()}}, \code{\link[=bm_list]{bm_list()}},  or \code{\link[=bm_font]{bm_font()}} object.}

\item{value}{Integer value for the new pixels.}

\item{top}{Number of pixels to shift towards the top side.}

\item{right}{Number of pixels to shift towards the right side.}

\item{bottom}{Number of pixels to shift towards the bottom side.}

\item{left}{Number of pixels to shift towards the left side.}
}
\value{
Either a \code{\link[=bm_bitmap]{bm_bitmap()}}, \code{\link[=bm_list]{bm_list()}}, or \code{\link[=bm_font]{bm_font()}} object.
}
\description{
Shifts non-padding elements within bitmaps by trimming on a specified side and padding on the other
while preserving the width and height of the original bitmap.
}
\details{
This function is a convenience wrapper around \code{\link[=bm_trim]{bm_trim()}} and \code{\link[=bm_extend]{bm_extend()}}.
}
\examples{
 font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
 font <- read_hex(font_file)
 capital_r <- font[[str2ucp("R")]]
 print(capital_r, px = c("-", "#"))
 capital_r <- bm_shift(capital_r, bottom = 2L, right = 1L)
 print(capital_r, px = c("-", "#"))
}
\seealso{
\code{\link[=bm_trim]{bm_trim()}} and \code{\link[=bm_extend]{bm_extend()}}
}
