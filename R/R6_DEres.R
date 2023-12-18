#' R6 Class DEres : DEG analysis result object
#' @description DEG analysis result object
#' @importFrom R6 R6Class
#' @importFrom LZ DEG_prepareGOglist
#' @importFrom LZ DEGp_Volcano2
#' @export
#' @author Jiang
DEres <- R6Class("DEres",
                 public = list(
                     #' @field resdf data.frame, all gene diff analysis result
                     resdf = NA,
                     #' @field glist.deg character vector, the significant deg(default criterion[p<0.05,q<0.1,abs(logfc)>1])
                     glist.deg = NA,
                     #' @field glist.deg.up character vector, the significant up deg(default criterion[p<0.05,q<0.1,logfc>1])
                     glist.deg.up = NA,
                     #' @field glist.deg.down character vector, the significant up deg(default criterion[p<0.05,q<0.1,logfc<-1])
                     glist.deg.down = NA,
                     #' @field fc.list list, different fold change,  list('1.2' = log2(1.2), '1.5' = log2(1.5), '2' = log2(2), '4' = log2(4), '8' = log2(8) )
                     fc.list = NA,
                     #' @field glist.deg.multi character vector, the significant deg by different criterion\cr
                     #' such as [p<0.01, q<0.1] logfc > log2(1.2) log2(1.5) 1 2 3 \cr
                     #'         [p<0.01, q<0.05] logfc > log2(1.2) log2(1.5) 1 2 3 \cr
                     #'         [p<0.01, q<0.01] logfc > log2(1.2) log2(1.5) 1 2 3 \cr
                     glist.deg.multi = NA,
                     #' @field glist.gsea name list, the all gene name list(name is genename, value is the log2FC)
                     glist.gsea = NA,
                     #' @field ddslist list, the diff analysis original result, the father of all_father
                     ddslist = NA,

                     #' @description Create a new DEres object.
                     #' @param resdf data.frame, all gene diff analysis result
                     #' @param glist.deg character vector, the significant deg(default criterion[p<0.05,q<0.1,abs(logfc)>1])
                     #' @param glist.deg.up character vector, the significant up deg(default criterion[p<0.05,q<0.1,logfc>1])
                     #' @param glist.deg.down character vector, the significant down deg(default criterion[p<0.05,q<0.1,logfc<-1])
                     #' @param fc.list list, different fold change,  list('1.2' = log2(1.2), '1.5' = log2(1.5), '2' = log2(2), '4' = log2(4), '8' = log2(8) )
                     #' @param glist.gsea name list, the all gene name list(name is genename, value is the log2FC)
                     #' @param ddslist list, the diff analysis original result, the father of all_father
                     initialize = function(resdf, glist.deg, glist.deg.up,
                                           glist.deg.down, fc.list, glist.gsea,
                                           ddslist) {
                         self$resdf <- resdf
                         self$glist.deg <- glist.deg
                         self$glist.deg.up <- glist.deg.up
                         self$glist.deg.down <- glist.deg.down
                         self$glist.gsea <- glist.gsea
                         self$fc.list <- fc.list
                         self$ddslist <- ddslist
                     },
                     #' @description update field glist.deg.multi
                     update.glist.deg.multi = function() {
                         g1 <- private$makeglist.multi(p = 0.05, fdr = 0.1)
                         g2 <- private$makeglist.multi(p = 0.05, fdr = 0.05)
                         g3 <- private$makeglist.multi(p = 0.01, fdr = 0.01)
                         self$glist.deg.multi <- c(g1, g2, g3)
                     },
                     #' @description plot valcano with mask gene using valcano2
                     #' @param filterc character the way of show picture
                     #' @param pvalue number pvalue, default 0.05
                     #' @param fdr number fdr, default 0.1
                     #' @param logfc.p number log2fc positive, default 1
                     #' @param logfc.n number log2fc negative, default -1
                     #' @param label_geneset character(n) the genelist need be shown in the valcano, default NULL
                     #' @param outdir character the picture outdir, default "result"
                     #' @param filename.base character the picture filename base, default "DEG_xx"
                     #' @param pic.w number the picture width, default 7
                     #' @param pic.h number the picture height, default 7
                     #' @param color.not character the picture color to not sig gene, default "#BEBEBE"
                     #' @param color.up character the picture color to up sig gene, default "#AC001D"
                     #' @param color.down character the picture color to down sig gene, default "#3D71AD"
                     #' @param ymax number the max ylim, default NULL
                     #' @param size number the picture point size, default 1.5 ...
                     #' @param shape number the picture point shape, default 16, or 1 ...
                     #' @param alpha number the transpate of picture, default 0.9
                     pic.valcano = function(filterc = "both",
                                            pvalue=0.05, fdr=0.1,
                                            logfc.p=1, logfc.n=-1,
                                            label_geneset = NULL,
                                            outdir="result",
                                            filename.base = "DEG_xx",
                                            pic.w = 7, pic.h =7,
                                            color.not = "#BEBEBE", color.up = "#AC001D",
                                            color.down = "#3D71AD", ymax = NULL, size=1.5,
                                            shape=16, alpha = 0.9) {
                         pic.l <- DEGp_Volcano2(self$resdf, filterc = filterc,
                                                pvalue=pvalue, fdr=fdr,
                                                logfc.p=logfc.p, logfc.n=logfc.n,
                                                label_geneset = label_geneset,
                                                outdir=outdir,
                                                filename.base = filename.base,
                                                pic.w = pic.w, pic.h = pic.h,
                                                color.not = color.not, color.up = color.up,
                                                color.down = color.down, ymax = ymax, size=size,
                                                shape=shape, alpha = alpha)
                         return(pic.l)
                     },
                     #' @description run GO analysis in batches(upgene, downgene, allgene) and write result to xlsx
                     #' @param genelist.filter number(n) or missing, select which fclist to run enrich anlaysis
                     #' @param outdir character, the output dir
                     #' @param glist.save logical default is TRUE, wether save genelist
                     #' @param rungo logical default is TRUE, wether run go analysis
                     #' @param runkegg logical default is TRUE, wether run kegg analysis
                     #' @param orgdb character deafult is "org.Hs.eg.db"
                     #' @param org_kegg character deafult is "hsa"
                     #' @param sigNodes number deafult is 20, go tree plot controls
                     #' @param rapid loggicla defaut is F, if set F, the function will plot go tree picture, which make cost much time
                     runENRICH = function(genelist.filter,
                                          outdir,
                                          glist.save = T,
                                          rungo = T,
                                          runkegg = T,
                                          rapid = T,
                                          orgdb = "org.Hs.eg.db",
                                          org_kegg='hsa',
                                          sigNodes=20) {
                         re <- private$lzENRICH(genelist.filter = genelist.filter,
                                                outdir = outdir,
                                                glist.save = glist.save,
                                                rungo = rungo,
                                                runkegg = runkegg,
                                                rapid = rapid,
                                                orgdb = orgdb,
                                                org_kegg=org_kegg,
                                                sigNodes=sigNodes)
                         return(re)
                     }
                 ),
                 private = list(
                     makeglist.multi = function(p = 0.05, fdr = 0.1) {
                         gogenelist <- lapply(self$fc.list, function(x) DEG_prepareGOglist(self$resdf, logfc = x, p, fdr))
                         names(gogenelist) <- paste0("p", p, "q", fdr, "_FC.", names(gogenelist))
                         return(gogenelist)
                     },
                     lzENRICH = function(genelist.filter,
                                         outdir,
                                         glist.save = T,
                                         rungo = T,
                                         runkegg = T,
                                         rapid = T,
                                         orgdb = "org.Hs.eg.db",
                                         org_kegg='hsa',
                                         sigNodes=20
                                         ) {
                         if (!missing(genelist.filter)) {
                             stopifnot(is.numeric(genelist.filter))
                         }
                         if (missing(genelist.filter)) {
                             genelist <- list('default' = list(
                                 'all' = self$glist.deg,
                                 'up' = self$glist.deg.up,
                                 'down' = self$glist.deg.down
                             ))
                         } else {
                             genelist <- self$glist.deg.multi[genelist.filter]
                         }
                         enrich <- DEG_runENRICH(genelist = genelist,
                                                 outdir = outdir,
                                                 glist.save = glist.save,
                                                 rungo = rungo, runkegg = runkegg,
                                                 rapid = runkegg,
                                                 orgdb = orgdb,
                                                 org_kegg=org_kegg,
                                                 sigNodes=sigNodes)
                         return(enrich)
                     }
                 )
)
