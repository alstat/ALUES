\name{landSuit}
\alias{landSuit}
\title{Land Suitability}
\usage{
landSuit(x, y, mf = "triangular", min = NULL, interval = "fixed")
}
\arguments{
  \item{x}{- data consisting the land units;}

  \item{y}{- data consisting the requirements or a given
  crop;}

  \item{mf}{- membership function, currently
  \code{'Triangular'}.}

  \item{min}{- minimum value for factors. If \code{NULL},
  \code{min} is set to 0.  To modify this simply set it to
  any numeric value, say 1 or 0.5, and so on.  For
  different \code{min} for every factor, simply concatenate
  the different \code{min}.}

  \item{interval}{- domains for every suitability class
  (S1, S2, S3).}
}
\description{
This function calculates the suitability scores and class
of the land units.
}
\examples{
library(ALUES)
x <- LandTerrain
y <- SoyaTerrainCR

SoyaTerrainSuit <- landSuit(x = x, y = y, mf = 'triangular')

# Extract the suitability score of first 10 land units
head(SoyaTerrainSuit$Suitability_Score, n = 10)

# Extract the suitability class of first 10 land units
head(SoyaTerrainSuit$Suitability_Class, n = 10)
}

