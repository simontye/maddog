\name{sourapple}
\alias{sourapple}
\title{Sour Apple}
\description{Returns the "Sour Apple" color.}
\usage{sourapple
}
\details{Returns the "Sour Apple" color.
}
\value{\code{maddog} returns the "Sour Apple" color.
}
\seealso{\code{\link{maddog_flavors}}}
\examples{
\donttest{
## Plot maddog palette
#ggplot() +
#  geom_point(data = iris, mapping = aes(x = Species, y = Sepal.Length, color = Species)) +
#  scale_color_manual(values = maddog("north", 3))
#  
## Plot maddog flavors
#ggplot() +
#  geom_point(data = iris, mapping = aes(x = Species, y = Sepal.Length, color = Species)) +
#  scale_color_manual(values = c(peachesandcream, purplerain, sourapple))
}
}
\references{http://www.ghettowine.com/maddog/
}
\author{Simon Tye}
\keyword{color}
