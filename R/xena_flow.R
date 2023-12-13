#' Create XENA OBJECT
#' @description The original xena data object.
#'
#' @param eset.count.file character the file dir
#' @param eset.fpkm.file character the file dir
#' @param os.file character the file dir
#' @param phen.file character the file dir
#' @param eset.count.info character the information
#' @param eset.fpkm.info character the information
#' @param os.info character the information
#' @param phen.info character the information
#' @param class character do not modifiy the param in xena flow
#'
#' @return `XENA` OBJECT
#' @export
#' @importFrom data.table fread
#' @include DataObject.R
#' @include R6_DEeset.R
#'
#' @author Jiang
XENADB.init <- function(eset.count.file, eset.fpkm.file, os.file, phen.file,
                        eset.count.info = "xena原始count数据(log2(x+1))",
                        eset.fpkm.info = "xena原始fpkm数据(log2(x+1))",
                        os.info = "xena原始OS数据",
                        phen.info = "xena原始phen数据",
                        class = "XENA") {
    XENADB <- YZ::Class(class = class)
    # eset.count
    if (missing(eset.count.file)) {
        stop("Error: the param eset.count.file must be set!")
        # XENADB$eset.count <- NA
    } else {
        eset.count <- data.table::fread(eset.count.file, data.table = F)
        XENADB$new(XENADB, "eset.count", eset.count, info = eset.count.info)
    }
    # eset.fpkm
    if (!missing(eset.fpkm.file)) {
        eset.fpkm <- data.table::fread(eset.fpkm.file, data.table = F)
        XENADB$new(XENADB, "eset.fpkm", eset.fpkm, info = eset.fpkm.info)
    }
    # os
    if (!missing(os.file)) {
        os <- data.table::fread(os.file, data.table = F)
        XENADB$new(XENADB, "os", os, info = os.info)
    }
    # phen
    if (!missing(phen.file)) {
        phen <- data.table::fread(phen.file, data.table = F)
        XENADB$new(XENADB, "phen", phen, info = phen.info)
    }
    return(XENADB)
}
