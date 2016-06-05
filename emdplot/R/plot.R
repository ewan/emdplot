#' The colours I use
#'@return A list of lists of colours, organized by darkness level
#'@export
emd_colours <- function() {
  list(
    lightest=list(
      turquoise="#D1EBE9",
      magenta="#FFE0D7",
      green="#C0D4A2",
      blue="#BBDDF0",
      yellow="#FFFE59",
      orange="#FFD26C"
    ),
    light=list(
      turquoise="#28D1C1",
      blue="#4E89B2",
      orange="#E69F00",
      brown="#B69070"
    ),
    dark=list(
      blue="#005EA2",
      maroon="#A44444"
    ),
    darkest=list(
      grey="#444444",
      maroon="#6A0000",
      purple="#370460"
    ))
}

#' A contrastive palette ranging from light to dark
#'
#' @param var A categorical variable with no more than eight unique values; if
#' it is a factor then the color palette will follow the order of the factor levels
#' from light to dark
#' @return A named vector of colours; if there are two colours
#' then they are orange (light) and blue (dark)
#' @export
emd_palette <- function(var) {
  if (!is.factor(var)) {
    var <- factor(var)
  }
  if (length(levels(var)) == 2) {
    result <- c(emd_colours()$light$orange, emd_colours()$dark$blue)
  } else if (length(levels(var)) == 3) {
    result <- c(emd_colours()$lightest$orange, emd_colours()$dark$blue,
                emd_colours()$darkest$grey)
  } else if (length(levels(var)) == 4) {
    result <- c(emd_colours()$lightest$orange,
                emd_colours()$light$turquoise,
                emd_colours()$dark$blue,
                emd_colours()$darkest$grey)
  } else if (length(levels(var)) == 5) {
    result <- c(emd_colours()$lightest$orange,  emd_colours()$light$turquoise,
                emd_colours()$dark$blue, emd_colours()$darkest$grey,
                emd_colours()$lightest$green)
  } else if (length(levels(var)) == 6) {
    result <- c(emd_colours()$lightest$orange,  emd_colours()$light$turquoise,
                emd_colours()$dark$blue, emd_colours()$darkest$grey,
                emd_colours()$lightest$green, emd_colours()$darkest$purple)
  } else if (length(levels(var)) == 7) {
    result <- c(emd_colours()$lightest$orange,  emd_colours()$light$turquoise,
                emd_colours()$dark$blue, emd_colours()$darkest$grey,
                emd_colours()$lightest$green,
                emd_colours()$dark$maroon, emd_colours()$darkest$purple)
  } else if (length(levels(var)) == 8) {
    result <- c(emd_colours()$lightest$orange,  emd_colours()$light$turquoise,
                emd_colours()$dark$blue, emd_colours()$darkest$grey,
                emd_colours()$lightest$green, emd_colours()$light$orange,
                emd_colours()$dark$maroon, emd_colours()$darkest$purple)
  }
  return(structure(result, names=levels(var)))
}

#' The theme I use for papers
#'
#' @param text_size Text size (default: 18)
#' @return A ggplot theme object
#' @export
emd_theme <- function(text_size=18) {
  return(theme_bw() + theme(text=element_text(size=text_size),
                            legend.position="bottom"))
}

#' Overlapping histograms with areas and a thick line
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
#' @export
hist_area_line <- function(x, group=NULL, var_measure_name="x", var_group_name="group",
                           colour_palette=emd_palette(group), line_width=3,
                           highlight_all=F, y_type="count", additional_vars=NULL) {
  y_str_sbin <- paste0("..", y_type, "..")
  if (!is.null(names(colour_palette))) {
    group <- factor(group, levels=names(colour_palette))
  }
  d <- data.frame(x=x, group=factor(group))
  if (!is.null(additional_vars) && is.data.frame(additional_vars) &&
      nrow(additional_vars) == nrow(d)) {
    d <- cbind(d, additional_vars)
  }
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
    d_lev <- d[d$group == lev,]
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

#' Overlapping histograms
#'
#' @param x A variable to plot
#' @param group grouping variable
#' @param colour_palette A vector containing the colours for each group; if this vector is named
#' then the order of the names will be used to order the groups on the plot
#' @param var_measure_name An display label for the dependent variable
#' @param var_group_name An display label for the group variable
#' @param line_width Line width
#' @return A ggplot plot object
#' @export
hist_overlapping <- function(x, group=NULL, var_measure_name="x", var_group_name="group",
                            colour_palette=emd_palette(group), line_width=3, bins=30,
                            additional_vars=NULL) {
  if (!is.null(names(colour_palette))) {
    group <- factor(group, levels=names(colour_palette))
  }
  d <- data.frame(x=x, group=factor(group))
  if (!is.null(additional_vars) && is.data.frame(additional_vars) &&
      nrow(additional_vars) == nrow(d)) {
    d <- cbind(d, additional_vars)
  }
  d <- d[!is.na(d$x),]
  p <- ggplot(d, aes(x=x, fill=group))
  p <- p + geom_histogram(position="identity", alpha=0.15, bins=bins)
  p <- p + scale_fill_manual(values=colour_palette, name=var_group_name,
                             breaks=levels(d$group))
  p <- p + geom_histogram(position="identity", aes(colour=group), bins=bins,
                          alpha=0, lwd=line_width)
  p <- p + geom_histogram(position="identity", aes(group=group), bins=bins,
                          alpha=0, lwd=min(1, line_width/2.), colour="black")
  p <- p + scale_colour_manual(values=colour_palette, name=var_group_name)
  p <- p + xlab(var_measure_name)
  p <- p + ylab("Count")
  return(p)
}
