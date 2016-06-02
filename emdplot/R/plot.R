LIGHT_ORANGE <- "#E69F00"
LIGHTEST_ORANGE <- "#FFD26C"
LIGHT_BROWN <- "#B69070"
DARK_BLUE <- "#005EA2"
LIGHT_BLUE <- "#4E89B2"
DARKEST_GREY <- "#444444"
LIGHTEST_YELLOW <- "#FFFE59"
DARKEST_MAROON <- "#6A0000"
DARK_MAROON <- "#A44444"
LIGHTEST_GREEN <- "#C0D4A2"
LIGHTEST_BLUE <- "#BBDDF0"
DARKEST_PURPLE <- "#370460"
LIGHTEST_MAGENTA <- "#FFE0D7"
LIGHT_TURQUOISE <- "#28D1C1"
LIGHTEST_TURQUOISE <- "#D1EBE9"

LIGHTEST_COLOURS <- c(LIGHTEST_ORANGE, LIGHTEST_YELLOW, LIGHTEST_GREEN, LIGHTEST_BLUE)
LIGHT_COLOURS <- c(LIGHT_TURQUOISE, LIGHT_ORANGE, LIGHT_BROWN, LIGHT_BLUE)
DARK_COLOURS <- c(DARK_BLUE, DARK_MAROON)
DARKEST_COLOURS <- c(DARKEST_GREY, DARKEST_PURPLE)
ALL_COLOURS <- c(LIGHTEST_COLOURS, LIGHT_COLOURS, DARK_COLOURS, DARKEST_COLOURS)

#' A contrastive palette ranging from light to dark; if there are two colours
#' then they are orange (light) and blue (dark)
#'
#' @param var A categorical variable with no more than eight unique values; if
#' it is a factor then the color palette will follow the order of the factor levels
#' from light to dark
emd_palette <- function(var) {
  if (!is.factor(var)) {
    var <- factor(var)
  }
  if (length(levels(var)) == 2) {
    result <- c(LIGHT_ORANGE, DARK_BLUE)
  } else if (length(levels(var)) == 3) {
    result <- c(LIGHTEST_ORANGE, DARK_BLUE, DARKEST_GREY)
  } else if (length(levels(var)) == 4) {
    result <- c(LIGHTEST_ORANGE, LIGHT_TURQUOISE, DARK_BLUE, DARKEST_GREY)
  } else if (length(levels(var)) == 5) {
    result <- c(LIGHTEST_ORANGE, LIGHT_TURQUOISE, DARK_BLUE, DARKEST_GREY,
                LIGHTEST_GREEN)
  } else if (length(levels(var)) == 6) {
    result <- c(LIGHTEST_ORANGE, LIGHT_TURQUOISE, DARK_BLUE, DARKEST_GREY,
                LIGHTEST_GREEN, DARKEST_PURPLE)
  } else if (length(levels(var)) == 7) {
    result <- c(LIGHTEST_ORANGE, LIGHT_TURQUOISE, DARK_BLUE, DARKEST_GREY,
                LIGHTEST_GREEN, DARK_MAROON, DARKEST_PURPLE)
  } else if (length(levels(var)) == 8) {
    result <- c(LIGHTEST_ORANGE, LIGHT_TURQUOISE, DARK_BLUE, DARKEST_GREY,
                LIGHTEST_GREEN, LIGHT_ORANGE, DARK_MAROON, DARKEST_PURPLE)
  }
  return(structure(result, names=levels(var)))
}

#' The theme I use for papers
#'
#' @param text_size Text size (default: 18)
#' @return A ggplot theme object
emd_theme <- function(text_size=18) {
  return(theme_bw() + theme(text=element_text(size=text_size),
                            legend.position="bottom"))
}

#' Returns a ggplot containing overlapping histograms for different groups, filled
#' in with areas and with a thick line over top
#'
#' @param x A variable to plot
#' @param group grouping variable
#' @param colour_palette A vector containing the colours for each group; if this vector is named
#' then the order of the names will be used to order the groups on the plot
#' @param var_measure_name An display label for the dependent variable
#' @param var_group_name An display label for the group variable
#' @param line_width Line width
#' @param highlight_all If TRUE, all groups get a thick line, otherise we leave off the top one
#' @param y_type "density", "ndensity", "count", or "ncount": density estimate, density estimate
#' scaled to maximum of one, count, or count normalized to maximum of one
#' @return A ggplot plot object
hist_area_line <- function(x, group=NULL, colour_palette=emd_palette(group),
                      var_measure_name="x", var_group_name="group",
                      line_width=3, highlight_all=F, y_type="count") {
  y_str_sbin <- paste0("..", y_type, "..")
  if (!is.null(names(colour_palette))) {
    group <- factor(group, levels=names(colour_palette))
  }
  d <- data.frame(x=x, group=factor(group))
  d <- d[!is.na(d$x),]

  p <- ggplot(d, aes(x=x))
  p <- p + stat_bin(aes_string(y=y_str_sbin, fill="group"),
                    position='identity', geom="area", colour="black", lwd=0.4*line_width)
  p <- p + scale_fill_manual(values=colour_palette, name=var_group_name,
                             breaks=levels(d$group))
  if (!highlight_all) {
    line_levs <- levels(d$group)[-length(levels(d$group))]
  } else {
    line_levs <- levels(d$group)
  }
  for (lev in line_levs) {
    d_lev <- dplyr::filter(d, group == lev)
    p <- p + stat_bin(data=d_lev, aes_string(y=y_str_sbin), position="identity",
                      geom="line", colour="black", lwd=line_width)
    p <- p + stat_bin(data=d_lev, aes_string(y=y_str_sbin), position="identity",
                      geom="line", colour=colour_palette[[lev]],
                      lwd=line_width*0.4)
  }
  p <- p + scale_colour_manual(values=colour_palette, name=var_group_name,
                               breaks=levels(d$group))
  p <- p + xlab(var_measure_name)
  if (y_type == "ndensity") {
    p <- p + ylab("Normalized Empirical Density")
  } else if (y_type == "density") {
    p <- p + ylab("Empirical Density")
  } else if (y_type == "count") {
    p <- p + ylab("Count")
  } else if (y_type == "ncount") {
    p <- p + ylab("Normalized Count")
  }
  return(p)
}