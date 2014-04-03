\name{landSuit}
\alias{landSuit}
\title{Land Suitability}
\usage{
landSuit(x, y, mf = "Triangular")
}
\arguments{
  \item{x}{- data consisting the land units;}

  \item{y}{- data consisting the requirements or a given
  crop;}

  \item{mf}{- membership function, currently
  \code{'Triangular'}.}
}
\description{
This function calculates the suitability scores and class
of the land units.
}
\examples{
library(ALUES)
x <- LandTerrain
y <- SoyaTerrainCR

SoyaTerrainSuit <- landSuit(x = x, y = y, mf = 'Triangular')

# Extract the suitability score of first 10 land units
head(SoyaTerrainSuit$score, n = 10)

# Extract the suitability class of first 10 land units
head(SoyaTerrainSuit$class, n = 10)
}

