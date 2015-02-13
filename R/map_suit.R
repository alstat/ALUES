#' Mapping Suitability Scores and Class
#' @export
#' 
#' @description
#' This function maps the scores and class of the suitability for all land units.
#' 
#' @param x an object of class suitability;
#' @param y a data frame consisting the properties of the land units;
#' @param location location of the land units, an address, longitude/latitude pair (in that order), or left/bottom/right/top bounding box
#' @param suit suitability type to be overlayed on the map, either \code{"scores"} or
#'        \code{"class"} of the land units. If \code{NULL}, scores of the suitability are mapped.
#' @param ovsuit if \code{TRUE}, the overall suitability of the land units are included in the map.
#' @param method,interval arguments of the \code{\link{overall_suit}}.
#' @param char characteristics to be mapped. If \code{NULL}, all characteristics are mapped.
#' @param cols number of columns in the layout of the map.
#' @param size size of the point overlayed in the map.
#' @param alpha transparency of the point overlayed in the map.
#' @param extent argument of the \code{\link{get_map}} function.
#' 
#' @param base_layer,
#' @param maprange, arguments of the \code{\link{ggmap}}
#'        function.
#' @param padding,darken
#' 
#' @param ... further argument for the \code{\link{get_map}} function.
#' 
#' @examples
#' library(ALUES)
#' x <- MarinduqueLT
#' y <- COCONUTSoilCR
#' coconut_tersuit <- suitability(x = x, y = y, interval = "unbias")
#' 
#' # Map suitability class
#' map_suit(x = coconut_tersuit, y = x, suit = "class", 
#'   location = c(121.999659, 13.378625), zoom = 11)
#'  
#' x <- MarinduqueWater
#' y <- RICEBRWaterCR
#' rice_watsuit <- suitability(x = x, y = y, 
#'   interval = "unbias", sow.month = 1)
#' 
#' # Map suitability scores
#' map_suit(x = rice_watsuit, y = x, suit = "scores", 
#'   location = c(121.999659, 13.378625), zoom = 11)
#' 
#' # Map suitability class
#' map_suit(x = rice_watsuit, y = x, suit = "class", 
#'   location = c(121.999659, 13.378625), zoom = 11)
#' 
#' # Map suitability scores with overall suitability
#' map_suit(x = rice_watsuit, y = x, suit = "scores", 
#'   ovsuit = TRUE, location = c(121.999659, 13.378625), zoom = 11)

map_suit <- function (x, y, location, suit = NULL, ovsuit = FALSE, method = NULL,
                      interval = NULL, char = NULL, cols = 2, size = 4, alpha = 1, extent = "panel", 
                      base_layer, maprange = FALSE, padding = 0.02,
                      darken = c(0, "black"), ...) {
  if (class(x) != "suitability")
    stop("x should be an object of class suitability.")
  if (!is.data.frame(y))
    stop("y should be a data.frame object.")
  
  if (is.character(location) && !is.numeric(location))
    loc <- get_map(as.character(location), ...)
  if (!is.character(location) && is.numeric(location))
    loc <- get_map(c(lon = location[1], lat = location[2]), ...)
  
  if (is.null(suit) || suit == "scores" && suit != "class") {
    if (!is.numeric(char) && is.null(char)) {
      val <- x[[2]]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = method, interval = interval)[, 1]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- rep(y$Lon, ncol(val)); d_map$Lat <- rep(y$Lat, ncol(val))
      p_map <- ggmap(loc, extent, base_layer, maprange, legend = "right", padding, darken) +
        geom_point(aes(x = Lat, y = Lon, colour = value), data = d_map, size = size, alpha = alpha) + 
        facet_wrap(~ Var2, ncol = cols) + 
        scale_colour_gradientn(name = "Scores", colours = c("red","yellow","#10E31E"))
      p_map
    } else if (is.numeric(char) && length(char) == 1) {
      val <- x[[2]][, char]
      val <- cbind(val)
      colnames(val) <- colnames(x[[2]])[char]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = method, interval = interval)[, 1]
      }
      d_map <- melt(val)
      d_map$Lon <- y$Lon; d_map$Lat <- y$Lat
      p_map <- ggmap(loc, extent, base_layer, maprange, legend = "right", padding, darken) +
        geom_point(aes(x = Lat, y = Lon, colour = value), data = d_map, size = size, alpha = alpha) + 
        facet_wrap(~ Var2, ncol = cols) + 
        scale_colour_gradientn(name = "Scores", colours = c("red","yellow","#10E31E"))
      p_map
    } else if (is.numeric(char) && length(char > 1)) {
      val <- x[[2]][, char]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = method, interval = interval)[, 1]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- rep(y$Lon, ncol(val)); d_map$Lat <- rep(y$Lat, ncol(val))
      p_map <- ggmap(loc, extent, base_layer, maprange, legend = "right", padding, darken) +
        geom_point(aes(x = Lat, y = Lon, colour = value), data = d_map, size = size, alpha = alpha) + 
        facet_wrap(~ Var2, ncol = cols) + 
        scale_colour_gradientn(name = "Scores", colours = c("red","yellow","#10E31E"))
      p_map
    }
  } else if (!is.null(suit) || suit != "scores" && suit == "class") { 
    if (!is.numeric(char) && is.null(char)) {
      val <- x[[3]]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = method, interval = interval)[, 2]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- rep(y$Lon, ncol(val)); d_map$Lat <- rep(y$Lat, ncol(val))
      p_map <- ggmap(loc, extent, base_layer, maprange, legend = "right", padding, darken) +
        geom_point(aes(x = Lat, y = Lon, colour = value), data = d_map, size = size, alpha = alpha) + 
        facet_wrap(~ Var2, ncol = cols) + scale_colour_discrete(name = "Class")
      p_map
    } else if (is.numeric(char) && length(char) == 1) {
      val <- x[[3]][, char]
      val <- data.frame(val)
      colnames(val) <- colnames(x[[2]])[char]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = method, interval = interval)[, 2]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- y$Lon; d_map$Lat <- y$Lat
      p_map <- ggmap(loc, extent, base_layer, maprange, legend = "right", padding, darken) +
        geom_point(aes(x = Lat, y = Lon, colour = value), data = d_map, size = size, alpha = alpha) + 
        facet_wrap(~ Var2, ncol = cols) + scale_colour_discrete(name = "Class")
      p_map
    } else if (is.numeric(char) && length(char > 1)) {
      val <- x[[3]][, char]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = method, interval = interval)[, 2]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- rep(y$Lon, ncol(val)); d_map$Lat <- rep(y$Lat, ncol(val))
      p_map <- ggmap(loc, extent, base_layer, maprange, legend = "right", padding, darken) +
        geom_point(aes(x = Lat, y = Lon, colour = value), data = d_map, size = size, alpha = alpha) + 
        facet_wrap(~ Var2, ncol = cols) + scale_colour_discrete(name = "Class")
      p_map
    }
  }
}