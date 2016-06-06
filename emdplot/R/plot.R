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
  if (!is.null(group)) {
    if (!is.null(names(colour_palette))) {
      group <- factor(group, levels=names(colour_palette))
    }
    d <- data.frame(x=x, group=factor(group))
  } else {
    d <- data.frame(x=x)
  }
  if (!is.null(additional_vars) && is.data.frame(additional_vars) &&
      nrow(additional_vars) == nrow(d)) {
    d <- cbind(d, additional_vars)
  }
  d <- d[!is.na(d$x),,drop=F]
  if (!is.null(group)) {
    p <- ggplot(d, aes(x=x, fill=group))
  } else {
    p <- ggplot(d, aes(x=x))
  }
  p <- p + geom_histogram(position="identity", alpha=0.15, bins=bins)
  if (!is.null(group)) {
    p <- p + scale_fill_manual(values=colour_palette, name=var_group_name,
                               breaks=levels(d$group))
    p <- p + geom_histogram(position="identity", aes(colour=group), bins=bins,
                            alpha=0, lwd=line_width)
    p <- p + geom_histogram(position="identity", aes(group=group), bins=bins,
                            alpha=0, lwd=min(1, line_width/2.), colour="black")
    p <- p + scale_colour_manual(values=colour_palette, name=var_group_name)
  } else {
    p <- p + geom_histogram(position="identity", colour=emd_colours()$dark$blue, bins=bins,
                            alpha=0, lwd=line_width)
    p <- p + geom_histogram(position="identity", bins=bins,
                            alpha=0, lwd=min(1, line_width/2.), colour="black")
  }
  p <- p + xlab(var_measure_name)
  p <- p + ylab("Count")
  return(p)
}

#'@export
linear_2x2_sumcode_barplot <- function(b0, bx1, bx2, bx1x2,
                                       x1="x1", x1A="A", x1B="B",
                                       x2="x2", x2A="A", x2B="B") {
  d0 <- rbind(data.frame(cell="Grand mean", prediction=b0, component="β0", type="Coef"),
              data.frame(cell="Grand mean", prediction=b0, component="Sum", type="Pred"))
  x1a_name <- paste0(x1, ": ", x1A)
  bx1_name <- paste0("β_", x1)
  dx1A <- do.call("rbind", list(data.frame(cell=x1a_name, prediction=b0, component="β0",type="Coef"),
                data.frame(cell=x1a_name, prediction=bx1, component=bx1_name, type="Coef"),
                data.frame(cell=x1a_name, prediction=b0+bx1, component="Sum", type="Pred")
                ))
  x1b_name <- paste0(x1, ": ", x1B)
  dx1B <- do.call("rbind", list(data.frame(cell=x1b_name, prediction=b0, component="β0", type="Coef"),
                data.frame(cell=x1b_name, prediction=-bx1, component=bx1_name, type="Coef"),
                data.frame(cell=x1b_name, prediction=b0-bx1, component="Sum", type="Pred")
                ))
  x2a_name <- paste0(x2, ": ", x2A)
  bx2_name <- paste0("β_", x2)
  dx2A <- do.call("rbind", list(data.frame(cell=x2a_name, prediction=b0, component="β0", type="Coef"),
                data.frame(cell=x2a_name, prediction=bx2, component=bx2_name, type="Coef"),
                data.frame(cell=x2a_name, prediction=b0+bx2, component="Sum", type="Pred")
                ))
  x2b_name <- paste0(x2, ": ", x2B)
  dx2B <- do.call("rbind", list(data.frame(cell=x2b_name, prediction=b0, component="β0", type="Coef"),
                data.frame(cell=x2b_name, prediction=-bx2, component=bx2_name, type="Coef"),
                data.frame(cell=x2b_name, prediction=b0-bx2, component="Sum", type="Pred")
                ))
  x1ax2a_name <- paste0(x1a_name, " &\n", x2a_name)
  bx1x2_name <- paste0("β_", x1, "_", x2)
  dx1Ax2A <- do.call("rbind", list(
    data.frame(cell=x1ax2a_name, prediction=b0,   type="Coef", component="β0"),
    data.frame(cell=x1ax2a_name, prediction=bx1,  type="Coef", component=bx1_name),
    data.frame(cell=x1ax2a_name, prediction=bx2,  type="Coef", component=bx2_name),
    data.frame(cell=x1ax2a_name, prediction=bx1x2,type="Coef", component=bx1x2_name),
    data.frame(cell=x1ax2a_name, prediction=b0+bx1+bx2+bx1x2, component="Sum", type="Pred")
    ))
  x1bx2a_name <- paste0(x1b_name, " &\n", x2a_name)
  dx1Bx2A <- do.call("rbind", list(
    data.frame(cell=x1bx2a_name, prediction=b0,    type="Coef", component="β0"),
    data.frame(cell=x1bx2a_name, prediction=-bx1,  type="Coef", component=bx1_name),
    data.frame(cell=x1bx2a_name, prediction=bx2,   type="Coef", component=bx2_name),
    data.frame(cell=x1bx2a_name, prediction=-bx1x2,type="Coef", component=bx1x2_name),
    data.frame(cell=x1bx2a_name, prediction=b0-bx1+bx2-bx1x2, component="Sum", type="Pred")
    ))
  x1ax2b_name <- paste0(x1a_name, " &\n", x2b_name)
  dx1Ax2B <- do.call("rbind", list(
    data.frame(cell=x1ax2b_name, prediction=b0,    type="Coef", component="β0"),
    data.frame(cell=x1ax2b_name, prediction=bx1,   type="Coef", component=bx1_name),
    data.frame(cell=x1ax2b_name, prediction=-bx2,  type="Coef", component=bx2_name),
    data.frame(cell=x1ax2b_name, prediction=-bx1x2,type="Coef", component=bx1x2_name),
    data.frame(cell=x1ax2b_name, prediction=b0+bx1-bx2-bx1x2, component="Sum", type="Pred")
    ))
  x1bx2b_name <- paste0(x1b_name, " &\n", x2b_name)
  dx1Bx2B <- do.call("rbind", list(
    data.frame(cell=x1bx2b_name, prediction=b0,   type="Coef", component="β0"),
    data.frame(cell=x1bx2b_name, prediction=-bx1, type="Coef", component=bx1_name),
    data.frame(cell=x1bx2b_name, prediction=-bx2, type="Coef", component=bx2_name),
    data.frame(cell=x1bx2b_name, prediction=bx1x2,type="Coef", component=bx1x2_name),
    data.frame(cell=x1bx2b_name, prediction=b0-bx1-bx2+bx1x2, component="Sum", type="Pred")
    ))
  d <- do.call("rbind", list(d0, dx1A, dx1B, dx2A, dx2B, dx1Ax2A, dx1Bx2A,
                             dx1Ax2B, dx1Bx2B))
  d$component <- factor(d$component, c("β0", bx1_name, bx2_name, bx1x2_name, "Sum"))
  d_pos <- d[d$prediction >= 0,]
  d_neg <- d[d$prediction < 0,]
  p <- ggplot()
  p <- p + geom_bar(data=d_pos, aes(x=type, fill=component, y=prediction),
                    stat="identity", position="stack", lwd=1, colour="black")
  p <- p + geom_bar(data=d_neg, aes(x=type, fill=component, y=prediction),
                    stat="identity", position="stack", lwd=1, colour="black")
  p <- p + geom_hline(yintercept=0, lwd=1.5)
  p <- p + scale_fill_manual(values=emd_palette(d$component), name="Model component")
  p <- p + facet_wrap(~ cell, ncol=9)
  p <- p + xlab("Case")
  p <- p + ylab("Model coefficient/prediction value")
  return(p)
}

#'@export
linear_2x2_sumcode_sum_barplot <- function(b0, bx1, bx2, bx1x2) {
  d0 <- data.frame(cell="Grand mean", prediction=b0)
  dx1A <- data.frame(cell="x1A", prediction=b0+bx1)
  dx1B <- data.frame(cell="x1B", prediction=b0-bx1)
  dx2A <- data.frame(cell="x2A", prediction=b0+bx2)
  dx2B <- data.frame(cell="x2B", prediction=b0-bx2)
  dx1Ax2A <- data.frame(cell="x1Ax2A", prediction=b0+bx1+bx2+bx1x2)
  dx1Bx2A <- data.frame(cell="x1Bx2A", prediction=b0-bx1+bx2-bx1x2)
  dx1Ax2B <- data.frame(cell="x1Ax2B", prediction=b0+bx1-bx2-bx1x2)
  dx1Bx2B <- data.frame(cell="x1Bx2B", prediction=b0-bx1-bx2+bx1x2)
  d <- do.call("rbind", list(d0, dx1A, dx1B, dx2A, dx2B, dx1Ax2A, dx1Bx2A,
                             dx1Ax2B, dx1Bx2B))
  p <- ggplot(data=d, aes(x=cell, y=prediction))
  p <- p + geom_bar(stat="identity", position="stack")
  p <- p + geom_hline(yintercept=0)
  return(p)
}

