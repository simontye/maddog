\name{maddog_palettes}
\alias{maddog_palettes}
\title{Prints all maddog color palettes}
\description{Prints all color palettes based on flavors of MD 20_20.}
\usage{maddog_palettes(n, type, select, exact.n)
}
\arguments{
\item{n}{set to NULL}
\item{type}{set to"div" for geographical palettes, "seq" for the rainbow palette, or "all" for all palettes}
\item{select}{set to NULL}
\item{exact.n}{set to TRUE}
}
\details{Prints all color palettes based on flavors of MD 20_20
}
\value{\code{maddog_palettes} prints all color palettes based on flavors of MD 20_20.
}
\seealso{\code{\link{maddog_palettes}}}
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
