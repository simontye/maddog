\name{maddog}
\alias{maddog}
\title{Maddog color palettes}
\description{Returns a color palette based on flavors of MD 20_20.}
\usage{maddog(name, n)
}
\arguments{
\item{name}{name of a color palette}
\item{n}{number of colors (variables)}
}
\details{Returns a color palette based on flavors of MD 20_20
}
\value{\code{maddog} returns a list of colors based on the chosen palette and number of variables. Palette options include "north", "south", "east", "west", and "rainbow".
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
