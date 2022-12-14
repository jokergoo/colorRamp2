\name{col2value}
\alias{col2value}
\title{
Convert back from colors to values
}
\description{
Convert back from colors to values
}
\usage{
col2value(r, g, b, col_fun)
}
\arguments{

  \item{r}{Red channel in \code{\link[colorspace]{sRGB}} color space. Value should be between 0 and 1.  The value can also be a character vector of colors or a three-column matrix with r, g, b as columns. In this case, \code{g} and \code{b} are ignored,}
  \item{g}{Green channel in \code{\link[colorspace]{sRGB}} color space. Value should be between 0 and 1.}
  \item{b}{Blue channel in \code{\link[colorspace]{sRGB}} color space. Value should be between 0 and 1.}
  \item{col_fun}{the color mapping function generated by \code{\link{colorRamp2}}.}

}
\details{
\code{\link{colorRamp2}} maps values to colors and this function does the reversed job.
Note for some color spaces, it cannot convert back to the original value perfectly.
}
\value{
A vector of original numeric values.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
x = seq(0, 1, length.out = 11)
col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
col = col_fun(x)
col2value(col, col_fun = col_fun)
col2value("red", col_fun = col_fun)

col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"), space = "sRGB")
col = col_fun(x)
col2value(col, col_fun = col_fun)
}
