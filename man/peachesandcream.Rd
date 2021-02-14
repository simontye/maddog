\name{peachesandcream}
\alias{peachesandcream}
\title{Peaches and Cream}
\description{Returns the "Peaches and Cream" color.}
\usage{peachesandcream
}
\details{Returns the "Peaches and Cream" color.
}
\value{\code{maddog} returns the "Peaches and Cream" color.
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
