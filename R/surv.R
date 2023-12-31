# shiny.readeset <- function(filename, gene1.col) {
#     # 对应shinyRCB中的shiny.readeset()函数
#     # xena的LZST2023数据
#     eset_clin <- fread(filename, data.table = F) # "data/tcga/DataClean_LUAD.tpm&os.csv.gz"
#     # 只保留01A数据
#     rindex <- str_sub(eset_clin[,1], 14, 16) == "01A"
#     eset_clin <- eset_clin[rindex, ]
#     # 根据病人去重
#     eset_clin[, 1] <- str_sub(eset_clin[,1], 1, 12)
#     eset_clin <- eset_clin %>% distinct(., sample_id, .keep_all = T)
#     # log化
#     eset_clin[, gene1.col:ncol(eset_clin)] <- log2(eset_clin[, gene1.col:ncol(eset_clin)] + 1)
#     return(eset_clin)
# }


#' Log2 dataframe start from the col
#' @description Log2 dataframe start from the col
#'
#' @param obj object this object must be a dataframe contain os data col and exprs data
#' @param log.col number the column where log2 transformate start
#'
#' @return dataframe
#' @export
#'
#' @examples #
#' #        sample_id  OS  OS.time     RAB4B
#' # TCGA-05-4249-01A   0     1523  8.796464
#' # TCGA-05-4250-01A   1      121  8.482698
#' # in this condition, the param log.col should be set 4, meaning log2(df[,4:end])
colstartlog2 <- function(obj, log.col=NULL) {
    # readxena.Classdata
    # 对应shinyRCB中的shiny.readeset()函数
    # xena Class data的对象数据，前N列为生存信息，后N列为表达数据，行为样本
    #          sample_id OS OS.time     RAB4B
    # 1 TCGA-05-4249-01A  0    1523  8.796464
    # 2 TCGA-05-4250-01A  1     121  8.482698
    eset_clin <- obj
    # log化
    if(!is.null(log.col)) {
        eset_clin[, log.col:ncol(eset_clin)] <- log2(eset_clin[, log.col:ncol(eset_clin)] + 1)
    }
    return(eset_clin)
}


#' xena.surv.cut
#'
#' @param eset_os.df dataframe
#' @param tagetGene character
#' @param method character
#' @importFrom survminer surv_cutpoint
#' @importFrom dplyr `%>%`
#' @importFrom stats fivenum
#'
#' @return list

xena.surv.cut <- function(eset_os.df, tagetGene="SIN3B", method=2) {
    df <- eset_os.df
    tagetGene.five <- fivenum(df[,tagetGene])
    if (method == "mean") {
        # mean
        cut.p <- mean(df[,tagetGene])
        df$Group <- as.vector(ifelse(df[,tagetGene] > cut.p, "h.mean","l.mean"))
    } else if (method == "upper75 vs low25") {
        # upper 75 vs low 25
        cut.p <- tagetGene.five[2]
        df$Group <- as.vector(ifelse(df[,tagetGene] > cut.p, "high75","low25"))
    } else if (method == "median") {
        # median
        cut.p <- tagetGene.five[3]
        df$Group <- as.vector(ifelse(df[,tagetGene] > cut.p, "h.median","l.median"))
    } else if (method == "upper25 vs low75") {
        # upper 25 vs low 75
        cut.p <- tagetGene.five[4]
        df$Group <- as.vector(ifelse(df[,tagetGene] > cut.p, "high25","low75"))
    } else if (method == "auto cut point") {
        # cut poion
        res.cut <- surv_cutpoint(df, #数据集
                                 time = "OS.time", #生存状态
                                 event = "OS", #生存时间
                                 variables = tagetGene) #需要计算的数据列名
        cut.p <- res.cut$cutpoint$cutpoint
        df$Group <- as.vector(ifelse(df[,tagetGene] > cut.p, "h.point","l.point"))
    } else if (method == "upper25 vs low25") {
        cut.p <- c(tagetGene.five[2], tagetGene.five[4])
        df.low25 <- df[df[,tagetGene] <= cut.p[1], ]
        df.low25$Group <- "low25"
        df.high25 <- df[df[,tagetGene] > cut.p[2], ]
        df.high25$Group <- "high25"
        df <- rbind(df.low25, df.high25) %>% data.frame(check.names = F)
    }
    return(list(eset_clin_p=df, cut.p=cut.p, five=tagetGene.five))
}


#' xena.surv.getPvale
#'
#' @param rt dataframe
#' @param num.tran number
#' @param main.text character
#'
#' @importFrom survival survdiff
#' @importFrom survival survfit
#' @importFrom stats pchisq
#' @importFrom ggsci pal_npg
#' @importFrom survminer ggsurvplot
#' @importFrom survminer theme_survminer
#'
#' @return list
xena.surv.getPvale <- function(rt, num.tran=365, main.text='') {
    # 指定group.name , 默认为"risk"
    rt$OS.time <- rt$OS.time/num.tran
    diff <- survdiff(Surv(OS.time, OS) ~ Group, data = rt)
    fit <- survfit(Surv(OS.time, OS) ~ Group, data = rt)
    pValue <- 1 - pchisq(diff$chisq,
                         df = (length(levels(as.factor(rt$Group))) - 1)
    )
    pValue <- signif(pValue, 4)
    # plot
    jco <- pal_npg()(9)[c(1,4,9)]
    ggs <- ggsurvplot(fit, # 创建的拟合对象
                      data = rt,  # 指定变量数据来源
                      palette = c(jco[1], jco[2]),
                      conf.int = F, # 显示置信区间
                      pval = T, # 添加P值
                      surv.median.line = "hv",  # 添加中位生存时间线
                      risk.table = TRUE, # 添加风险表
                      xlab = "Time in Years", # 指定x轴标签
                      legend = c(0.8,0.8), # 指定图例位置
                      legend.title = main.text, # 设置图例标题
                      #legend.labs = 1:5, # 指定图例分组标签
                      tables.y.text = F,
                      break.x.by = 2, # 设置x轴刻度间距
                      ggtheme = theme_survminer(font.legend = c(11, "plain", "black")),
                      tables.theme = theme_survminer(font.main = 11) )
    ggs$plot <- ggs$plot +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0))
    return(list(pValue=pValue, fit=fit, pic=ggs))
}


#' Xena surv pipeline
#' @description km surv analysis, km plot + density
#'
#' @param eset_os data.frame a dataframe contain os info and exprs(row is sample)
#' @param tagetGene character genes
#' @param method character cut point
#'
#' @return plot
#' @export
#'
#' @examples #
lzsurvkm <- function(eset_os, tagetGene, method) {
    eset_os.cut <- xena.surv.cut(eset_os.df = eset_os, tagetGene=tagetGene,
                                 method=method)
    ps <- xena.surv.getPvale(rt = eset_os.cut$eset_clin_p, num.tran = 365,
                             main.text = tagetGene)
    return(list(eset_os.cut= eset_os.cut, pic = ps))
}


#' ggtheme
#' @description mytheme_prism
#' @import ggplot2
#'
#' @return ggtheme
#' @export
#'
#' @examples #
mytheme_prism <- function() {
    # max, by=5 # breaks = seq(0, max, by = by) ##limits = c(0, 40),
    list(
        scale_y_continuous(expand = c(0,0)),
        scale_x_continuous(expand = c(0,0)),
        #ggprism::theme_prism()
        theme_classic()
    )
}


#' plot density
#'
#' @param data dataframe expr dataframe
#' @param col character the taget gene name
#' @import ggplot2
#' @importFrom stats median
#'
#' @return density plot
#' @export
#'
#' @examples #
LZplot.dens <- function(data, col) {
    five <- fivenum(data[, col])
    ggplot(data, aes(x = data[, col])) +
        geom_density(fill = "gray", alpha = 0.5) +
        geom_vline(xintercept = five, col = "blue", size=1) +
        geom_vline(xintercept = median(data[, col]), col = "red", size=1) +
        geom_vline(xintercept = mean(data[, col]), col = "yellow", size=1, lty=2) +
        annotate("text", x = five, y = 0,
                 label = c(paste0("Min:", round(five[1],2)),
                           paste0("Q1:", round(five[2],2)),
                           paste0("Median:", round(five[3],2)),
                           paste0("Q3:", round(five[4],2)),
                           paste0("Max:", round(five[5],2)) ),
                 color = c("black", "black", "red", "black", "black"),
                 hjust = 1, vjust = c(-1,-2.5,-4.5,-6,-8)) +
        annotate("text", x = mean(data[, col]), y = 0,
                 label = paste0("Mean:", round(mean(data[, col]))), color="yellow",
                 hjust = 1, vjust = -1) +
        mytheme_prism() + xlab(label = col) + ylab(label = NULL)
}
#ggplot(eset_clin, aes(x=PIGV)) + geom_density(fill="grey")
#LZplot.dens(eset_clin, "PIGV")
