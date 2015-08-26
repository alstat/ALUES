#' Mapping Suitability Scores and Class
#' @export
#' 
#' @description
#' Maps the scores and classes of the suitability for all land units.
#' 
#' @param x an object of class suitability;
#' @param y a data frame consisting the properties of the land units;
#' @param country geographical location of the land units. Specifically,
#'        the country's ISO code where it is located.
#' @param province lower-level subdivision of the geographical location of the land units.
#' @param suit suitability type to be overlayed on the map, either \code{"score"} or
#'        \code{"class"} of the land units. If \code{NULL}, scores of the suitability are mapped.
#' @param ovsuit if \code{TRUE}, the overall suitability of the land units is included in the map.
#' @param ovs_arg a list object containing two entries namely, \code{method} and \code{interval}, 
#'        which are arguments of the \code{\link{overall_suit}} function.
#' @param char characteristic(s)/factor(s) of the land units to be mapped. If \code{NULL}, 
#'        all factors are plotted. So for the first factor of the land units,
#'        assign \code{char = 1}; for first two factors of the land units, assign
#'        \code{char = 1:2} or \code{char = c(1, 2)}; and so on. Note that, the number of 
#'        factors depend on the number of characteristics in the crop requirements that
#'        matches with the characteristics of the land units.
#' @param fill color of the map.
#' @param shadow color of the drop shadow of the map
#' @param ncol number of columns in the layout of the map.
#' @param size size of the points overlayed in the map.
#' @param alpha transparency of the points overlayed in the map.
#' @param text_opts a list object containing the options for configuring the style of the names of the 
#'        regions in the map.
#' @param labels a list object containing the title and axes labels.
#' @param plot_theme theme of the ggplot, if set \code{NULL}, the plot will have a default
#'        ggplot theme. Other options are \code{"bw"}, \code{"gray"}, \code{"linedraw"},
#'        \code{"light"}, \code{"light"}, \code{"minimal"}, and \code{"classic"}.
#' @param plot_theme_opts a list object with two entries (\code{base_size} - size 
#'        of the font in the plot, and \code{base_family} - font family). These are
#'        options of the \code{plot_theme}. Say if \code{plot_theme="bw"}, then
#'        the plot will have a black and white theme, in which the size of the font
#'        in the plot is defined by the \code{base_size} entry of the list of 
#'        \code{plot_theme_opts}, and the same goes to the font family in the plot,
#'        is defined by the \code{base_family} entry of the list of \code{plot_theme_opts}
#' @param ... further arguments of the \code{\link{theme}} function of \code{ggplot2}.
#'        If \code{plot_theme} argument is not \code{NULL}, then these further arguments
#'        for the \code{\link{theme}} function of the ggplot2 will not take effect.
#' 
#' @details 
#' The function depends on ggplot2 package, thus themes of the map can be configured
#' using the \code{\link{theme}} function, and other operations in ggplot2.
#' 
#' @seealso
#' \code{\link{suitability}}, \code{\link{overall_suit}}.
#' 
#' @examples
#' library(ALUES)
#' # Compute the suitability of the Soil and Terrain characteristics of the
#' # land units in Marinduque province for soil of coconut.
#' coconut_tersuit <- suitability(x = MarinduqueLT, y = COCONUTSoilCR, 
#'   interval = "unbias")
#' 
#' # Map the suitability score of the land units
#' map_suit(x = coconut_tersuit, y = MarinduqueLT, 
#'   country = "PHL", province = "Marinduque",
#'   suit = "score")
#'   
#' # Map the suitability class of the land units
#' map_suit(x = coconut_tersuit, y = MarinduqueLT, 
#'   country = "PHL", province = "Marinduque",
#'   suit = "class")
#'   
#' # Compute the suitability of the Water Characteristics of the 
#' # land units in Marinduque province for Rainfed Bunded Rice
#' rice_watsuit <- suitability(x = MarinduqueWater, y = RICEBRWaterCR, 
#'   interval = "unbias", sow.month = 1)
#' 
#' # Map the suitability scores of the land units
#' map_suit(x = rice_watsuit, y = MarinduqueWater, 
#'   country = "PHL", province = "Marinduque")
#' 
#' # Map the suitability class of the land units
#' map_suit(x = rice_watsuit, y = MarinduqueWater, 
#'   country = "PHL", province = "Marinduque",
#'   suit = "class")
#' 
#' # Map the suitability class of the land units along with 
#' # the overall suitability
#' map_suit(x = rice_watsuit, y = MarinduqueWater, 
#'   country = "PHL", province = "Marinduque",
#'   suit = "class", ovsuit = TRUE, ncol = 3)
#' 
#' # ... - further option for theme function of ggplot2 can be use
#' # to personalize the design of the plot
#' map_suit(x = coconut_tersuit, y = MarinduqueLT, country = "PHL", 
#'   province = "Marinduque", suit = "score", 
#'   strip.text = element_text(size = 20, colour = "red", family = "serif", 
#'   lineheight = 3), 
#'   strip.background = element_rect(fill = "#FFBD2F"))
#'   
#' # Using plot_theme option
#' # Map the suitability score of the land units
#' map_suit(x = coconut_tersuit, y = MarinduqueLT, country = "PHL", 
#'   province = "Marinduque",
#'   suit = "score", plot_theme = "classic", 
#'   plot_theme_opts = list(base_size = 20, base_family = "serif"))
#' 
#' # Above code is equivalent to the following code
#' p <- map_suit(x = coconut_tersuit, y = MarinduqueLT, country = "PHL", province = "Marinduque", suit = "score")
#' p + theme_classic(base_size = 20, base_family = "serif")
map_suit <- function (x, y, country = "PHL", province, suit = NULL, ovsuit = FALSE,
                      ovs_arg = list(method = "minimum", interval = NULL), char = NULL,
                      fill = "#FFF7BC", shadow = "#9ECAE1", ncol = 2, size = 3, alpha = 1,
                      text_opts = list(alpha = 1, angle = 0, colour = "black", family = "sans", fontface = 1, lineheight = 1, size = 4),
                      labels = list(title = "", xlab = "", ylab = ""), plot_theme = NULL, 
                      plot_theme_opts = list(base_size = 12, base_family = "sans"), ...) {
  if (class(x) != "suitability")
    stop("x should be an object of class suitability.")
  if (!is.data.frame(y))
    stop("y should be a data.frame object.")
  
  map_lvl0 <- getData("GADM", country = country, level = 0)
  map_lvl2 <- getData("GADM", country = country, level = 2)
  
  prov <- map_lvl2[map_lvl2$NAME_1 == as.character(province),]
  munic_coord <- coordinates(prov)
  munic_coord <- data.frame(munic_coord)
  munic_coord$label <- prov@data$NAME_2
  
  if (is.null(suit) || suit == "score" && suit != "class") {
    if (!is.numeric(char) && is.null(char)) {
      val <- x[[2]]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = ovs_arg$method, interval = ovs_arg$interval)[, 1]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- rep(y$Lon, ncol(val)); d_map$Lat <- rep(y$Lat, ncol(val))
    } else if (is.numeric(char) && length(char) == 1) {
      val <- x[[2]][, char]
      val <- cbind(val)
      colnames(val) <- colnames(x[[2]])[char]
      val <- as.data.frame(val)
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = ovs_arg$method, interval = ovs_arg$interval)[, 1]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- y$Lon; d_map$Lat <- y$Lat
    } else if (is.numeric(char) && length(char) > 1) {
      val <- x[[2]][, char]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = ovs_arg$method, interval = ovs_arg$interval)[, 1]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- rep(y$Lon, ncol(val)); d_map$Lat <- rep(y$Lat, ncol(val))
    }
    p1 <- ggplot() + geom_polygon(data = prov, aes(long + 0.008, lat - 0.005, group = group), fill = shadow) + 
      geom_polygon(data = prov, aes(long, lat, group = group), colour = "grey10", fill = fill) +
      geom_tile(aes(x = Lat, y = Lon, fill = value), data = d_map, size = size, alpha = alpha) + 
      facet_wrap(~ Var2, ncol = ncol) + 
      geom_polygon(data = prov, aes(long, lat, group = group), colour = "#4E4E4C", alpha = 0) +
      geom_text(data = munic_coord, aes(x = X1, y = X2, label = label), alpha = text_opts$alpha,
                angle = text_opts$angle, colour = text_opts$colour, family = text_opts$family,
                fontface = text_opts$fontface,
                lineheight = text_opts$lineheight, size = text_opts$size) +
      coord_equal() + ggtitle(as.character(labels$title)) + xlab(as.character(labels$xlab)) + ylab(as.character(labels$ylab)) +
      #scale_colour_gradientn(name = "Scores", colours = c("red","yellow","#10E31E")) +
      theme(axis.text.y = element_text(angle = 90, hjust = 0.5), ...)
    if (!is.null(plot_theme) && plot_theme == "bw") {
      p1 + theme_bw(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "gray") {
      p1 + theme_gray(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "light") {
      p1 + theme_light(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "linedraw") {
      p1 + theme_linedraw(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "minimal") {
      p1 + theme_minimal(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "classic") {
      p1 + theme_classic(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else {
      p1
    }
  } else if (!is.null(suit) || suit != "score" && suit == "class") {
    if (!is.numeric(char) && is.null(char)) {
      val <- x[[3]]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = ovs_arg$method, interval = ovs_arg$interval)[, 2]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- rep(y$Lon, ncol(val)); d_map$Lat <- rep(y$Lat, ncol(val))
    } else if (is.numeric(char) && length(char) == 1) {
      val <- x[[3]][, char]
      val <- data.frame(val)
      colnames(val) <- colnames(x[[2]])[char]
      val <- as.data.frame(val)
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = ovs_arg$method, interval = ovs_arg$interval)[, 2]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- y$Lon; d_map$Lat <- y$Lat
    } else if (is.numeric(char) && length(char) > 1) {
      val <- x[[3]][, char]
      if (ovsuit == TRUE) {
        val$Overall <- overall(x[[2]], y = x, method = ovs_arg$method, interval = ovs_arg$interval)[, 2]
      }
      d_map <- melt(as.matrix(val))
      d_map$Lon <- rep(y$Lon, ncol(val)); d_map$Lat <- rep(y$Lat, ncol(val))
    }
    p1 <- ggplot() + geom_polygon(data = prov, aes(long + 0.008, lat - 0.005, group = group), fill = shadow) + 
      geom_polygon(data = prov, aes(long, lat, group = group), colour = "grey10", fill = fill) +
      geom_tile(aes(x = Lat, y = Lon, fill = value), data = d_map, size = size, alpha = alpha) + 
      facet_wrap(~ Var2, ncol = ncol) + 
      geom_polygon(data = prov, aes(long, lat, group = group), colour = "#4E4E4C", alpha = 0) +
      geom_text(data = munic_coord, aes(x = X1, y = X2, label = label), alpha = text_opts$alpha,
                angle = text_opts$angle, colour = text_opts$colour, family = text_opts$family,
                fontface = text_opts$fontface,
                lineheight = text_opts$lineheight, size = text_opts$size) +
      coord_equal() + ggtitle(as.character(labels$title)) + xlab(as.character(labels$xlab)) + ylab(as.character(labels$ylab)) +
      scale_colour_discrete(name = "Class") +
      theme(axis.text.y = element_text(angle = 90, hjust = 0.5), ...)
    if (!is.null(plot_theme) && plot_theme == "bw") {
      p1 + theme_bw(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "gray") {
      p1 + theme_gray(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "light") {
      p1 + theme_light(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "linedraw") {
      p1 + theme_linedraw(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "minimal") {
      p1 + theme_minimal(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else if (!is.null(plot_theme) && plot_theme == "classic") {
      p1 + theme_classic(base_size = plot_theme_opts$base_size, base_family = plot_theme_opts$base_family)
    } else {
      p1
    }
  }
}