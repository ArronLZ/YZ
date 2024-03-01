#' Update ggplot theme text
#' @description Update ggplot theme text
#'
#' @param size number font size, default is 14
#' @param color character font color, default is "black", can be rgb value, such as "#00FFFF"
#' @param family character font family, default is "serif"
#' @importFrom ggplot2 theme
#'
#' @return ggtheme
#' @export
#' @author Jiang
ggtheme.update.text <- function(size = 14, color = "black", family = "serif") {
    list(
        theme(
            axis.text = element_text(size = size, color = color, family = family),
            title = element_text(size = size, face = "bold", family = family))
    )
}

#' Update ggplot theme text legend
#' @description Update ggplot theme text
#'
#' @param size number font size, default is 8
#' @param color character font color, default is "black", can be rgb value, such as "#00FFFF"
#' @param family character font family, default is "serif"
#' @importFrom ggplot2 theme
#'
#' @return ggtheme
#' @export
#' @author Jiang
ggtheme.update.text.legend <- function(size = 8, color = "black", family = "serif") {
    list(
        theme(
            legend.text = element_text(size = size, color = color, family = family)
    )
    )
}


#' Add ggplot theme stat
#' @description Add ggplot theme stat
#'
#' @param label.y numner the stat's y, for example, max(p.data[, taget]), can aslo be a numeric(n) as c(29, 35, 40) for multi compare
#' @param label character the stat's show method, such as "p.signif", "p.format", ...
#' @param method character stat method, such as "t.test", "wilcox.test", "anova", "kruskal.test"
#' @param comparisons list multi compare group, list(c("0.5","1"), c("1", "2"),c("0.5", "2"))
#' @param label.x numner the stat's x
#' @param ref.group character the ref group
#'
#' @importFrom ggpubr stat_compare_means
#'
#' @return ggtheme
#' @export
#' @examples NA
#' @author Jiang
ggtheme.add.stat <- function(label.x = 1.45, label.y,  label = "p.signif", method,
                             comparisons, ref.group) {
    list(
        stat_compare_means(label.y = label.y, label = label, label.x = label.x,
                           method = method, comparisons = comparisons, ref.group = ref.group,
                           )
    )
}

#' Set ggplot theme
#' @description et ggplot theme y axis
#'
#' @return ggtheme
#' @export
#' @examples NA
#' @author Jiang
ggtheme.set.y<- function() {
    list(
        scale_y_continuous(#breaks = seq(-1000, 3000, 1000),
            expand = c(0, 0),
            limits = c(5, 16) )
    )
}

