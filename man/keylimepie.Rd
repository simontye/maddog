\name{keylimepie}
\alias{keylimepie}
\title{Key Lime Pie}
\description{Returns the "Key Lime Pie" color.}
\usage{keylimepie
}
\details{Returns the "Key Lime Pie" color.
}
\value{\code{maddog} returns the "Key Lime Pie" color.
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