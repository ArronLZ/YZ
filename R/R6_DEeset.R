#' R6 Class DEeset : DEG analysis object
#' @description DEG analysis pipeline object
#' @importFrom R6 R6Class
#' @importFrom LZ DEG_DESeq2.dds
#' @importFrom LZ DEG_DESeq2.pca
#' @importFrom LZ DEG_DESeq2.ana
#' @importFrom LZ DEGres_ToRICH
#' @importFrom dplyr arrange
#' @include R6_DEres.R
#' @export
#' @author Jiang
DEeset <- R6Class("DEeset",
                  public = list(
                      #' @field mark character the object name.
                      mark=NA,
                      #' @field eset dataframe a exprs eset dataframe, \cr
                      #'  col: gene, rownames: samples
                      eset=NA,
                      #' @field eset2 dataframe a clean dataframe from `eset` by updateGroup for different gene anlaysis\cr
                      #'  col: gene, rownames: samples
                      eset2 = NA,
                      #' @field group dataframe a phen dataframe for different gene anlaysis, \cr
                      #'  the first col: Type, rownames: samples
                      group=NA,
                      #' @field os dataframe os data, rowname: samples, col: OS OS.time
                      os=NA,
                      #' @field phen dataframe phen data, rowname: samples, col: age, sex, stage ...
                      phen=NA,
                      #' @field fc.list list, different fold change,  list('1.2' = log2(1.2), '1.5' = log2(1.5), '2' = log2(2), '4' = log2(4), '8' = log2(8) )
                      fc.list = list('1.2' = log2(1.2), '1.5' = log2(1.5),
                                     '2' = log2(2), '4' = log2(4), '8' = log2(8)),

                      #' @description
                      #' Create a new DEeset object.
                      #' @param mark character the object name.
                      #' @param eset dataframe a exprs eset dataframe for different gene anlaysis, \cr
                      #'  col: gene, rownames: samples
                      #' @param group dataframe a phen dataframe for different gene anlaysis, \cr
                      #'  the first col: Type, rownames: samples
                      #' @param os dataframe os data, rowname: samples, col: OS OS.time
                      #' @param phen dataframe phen data, rowname: samples, col: age, sex, stage ...
                      #' @return A new `DEeset` object
                      initialize = function(mark, eset, group=NULL,
                                            os=NULL, phen=NULL) {
                          suppressWarnings({
                              if (!is.null(group)) private$validity(eset, group)
                              if (!is.null(os)) private$validity(eset, os)
                              if (!is.null(phen)) private$validity(eset, phen)
                          })
                          self$mark <- mark
                          self$eset <- eset
                          self$group <- group
                          self$os <- os
                          self$phen <- phen
                          # private$test <- test # 给私有属性赋值
                      },
                      #' @description
                      #' test
                      hello = function() {
                          print(paste("hello", self$mark))
                          # private$runtest(self$mark) # 调用私有方法
                      },
                      #' @description updateGroup
                      #' @param suffix1 character the match rule1: treatment group suffix
                      #' @param suffix2 character the match rule2: control group suffix
                      #' @param endT character if match true
                      #' @param endF character if match false
                      #' @return no return, just update object's field group
                      updateGroup = function(suffix1 = "01A", suffix2 = "11A",
                                             endT="Tumor", endF="Normal") {
                          group <- private$esetMakeGroup(eset = self$eset,
                                                         suffix1 = suffix1,
                                                         suffix2 = suffix2,
                                                         endT=endT, endF=endF)
                          group <- dplyr::arrange(group, Type)
                          self$group <- group
                          self$eset2 <- self$eset[, rownames(group)]
                      },
                      #' DEG analysis pipleline
                      #' @description DEG analysis pipeline, DESeq2 deg analysis, plot pca, prepare GSEA
                      #' @param f_mark character  a mark show what analysis you are doning, \cr
                      #'  it will be used for subdir of outdir or filename
                      #' @param outdir character outdir
                      #' @param pval number criteria of DEG analysis: Pvalue, defaut is 0.05
                      #' @param fdr number criteria of DEG analysis: FDR/Qvalue, defaut is 0.1
                      #' @param logfc  number criteria of DEG analysis: log2FC, defaut is 1
                      #' @param method  character deg analysis method, default is "deseq2", can also be "edger" or "voom"
                      #' @param coef  number default is 2, do not modify this parameter unless you know what you are doing!
                      #' @return a list (deg analysis)
                      runDEG = function(f_mark = "DE", outdir = "result",
                                        pval = 0.05, fdr=0.1, logfc=1,
                                        method = "deseq2", coef = 2) {
                          #glist <- list(eset = self$eset, group = self$group, f_mark = f_mark)
                          #DEG_edgeR(exprset.group, pval=pval, fdr=fdr, logfc=logfc, coef = coef)
                          #DEG_voom(exprset.group, pval=fdr, fdr=fdr, logfc=logfc)
                          res_deg_ana <- private$deg_ana(f_mark = f_mark,
                                                        outdir = outdir,
                                                        pval=pval,
                                                        fdr=fdr,
                                                        logfc=logfc,
                                                        method = method,
                                                        coef = coef)
                          DEres <- DEres$new(resdf = res_deg_ana$DIFF.ALL,
                                             glist.deg = res_deg_ana$DEG.ALL$DEG.ALL,
                                             glist.deg.up = res_deg_ana$UP$UP,
                                             glist.deg.down = res_deg_ana$DOWN$DOWN,
                                             fc.list = self$fc.list,
                                             glist.gsea = res_deg_ana$gsealist,
                                             ddslist = res_deg_ana$diffan.obj)
                          return(DEres)
                      }
                  ),
                  private = list(
                      # outdir <- "result",
                      # test = NA,
                      # runtest = function(canshu) {
                      #   print(paste(canshu, "and", private$test))
                      # },
                      validity = function(eset, group){
                          if (!all(names(eset) == rownames(group))) {
                              stop("The eset's colnames must be equal to rownames of phen")
                          }
                      },
                      esetMakeGroup = function(eset, suffix1 = "01A", suffix2 = "11A",
                                               endT="Tumor", endF="Normal") {
                          type <- ifelse(grepl(paste0("(", suffix1, "|", suffix2, ")$"), colnames(eset)), #endsWith(colnames(eset), suffix),
                                         ifelse(grepl(paste0(suffix1, "$"), colnames(eset)), endT, endF), NA)
                          group <- data.frame(row.names = colnames(eset),
                                              Type = type, check.names = F)
                          group <- na.omit(group)
                          group$Type <- as.factor(group$Type)
                          return(group)
                      },
                      deg_ana = function(f_mark = "DE", outdir = "result",
                                         pval, fdr, logfc,
                                         method = "deseq2", coef = 2) {
                          outdirsub <- paste0(outdir, "/", f_mark)
                          outdirsub.gsea <- paste0(outdirsub, "/gsea")
                          outdirsub.rich <- paste0(outdirsub, "/rich")
                          glist <- list(eset = self$eset2, group = self$group,
                                        f_mark = f_mark)
                          if (method == "deseq2") {
                              dds <- DEG_DESeq2.dds(exprset.group=glist, batch = F)
                              DEG_DESeq2.pca(dds, outdir = outdirsub) # 此处有warning信息，不用管。
                              dds_list <- DEG_DESeq2.ana(dds,  pval = pval, fdr = fdr, logfc = logfc)
                          }
                          if (method == "edger") {
                              dds_list <- DEG_edgeR(glist, pval=pval, fdr=fdr, logfc=logfc, coef = coef)
                          }
                          if (method == "voom") {
                              dds_list <- DEG_voom(glist, pval=pval, fdr=fdr, logfc=logfc)
                          }
                          # 差异后分析
                          all_father <- DEGres_ToRICH(diffan.obj = dds_list,
                                                      p=pval, q=fdr, f=logfc,
                                                      mark=f_mark,
                                                      outdir = outdirsub.rich)
                          # 构建GSEA官网软件分析所需格式文件
                          DEGres_ToGSEA(diffan.obj = dds_list, outdir = outdirsub.gsea) # 此处有warning，不用管。
                          return(all_father)
                      }
                  )
)
