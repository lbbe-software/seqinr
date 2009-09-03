\name{col2alpha}
\Rdversion{1.1}
\alias{col2alpha}
\title{To use a standard color with an alpha transparency chanel}
\description{
Takes as input a standard R color and an alpha value to return
its rgb coding.
}
\usage{
col2alpha(color, alpha = 0.5)
}
\arguments{
  \item{color}{A standard R color as in \code{\link{colors}}.}
  \item{alpha}{An alpha transparency value in the interval [0,1].}
}
\value{same as in \code{\link{rgb}}.}
\author{J.R. Lobry}
\seealso{\code{\link{colors}}, \code{\link{col2rgb}}, \code{\link{rgb}}.}
\examples{
par(mar = c(0, 0, 2, 2)+0.1, oma = c(0, 0, 2, 0), mfrow = c(3,2))
for(testcol in c("blue", "red", "green", "yellow", "purple", "darkgreen")){
  plot(0,0, type="n", xlim=0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "", main = testcol)
  n <- 11
  for(i in seq(0, 1, length = n)){
    col <- col2alpha(testcol, i)
    rect(i, 0, i + 1/n, 1, col = col, border = "black", xpd = NA)
    text(i+0.5/n, 0.5, round(i,2), xpd = NA)
  }
}
mtext("Effect of alpha on some colors", side = 3, outer = TRUE)
}
