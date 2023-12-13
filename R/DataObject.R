#' Set an new DataClass(environment)
#' @description Set an new environment to store important data in a project
#' @param class character class name
#' @return enviroment(DataClass)
#' @export
#'
#' @examples
#' # # create DATA
#' # AA <- Class(class = "AA")
#' #
#' # # list info
#' # AA$catinfo()
#' #
#' # # add variable
#' # AA$new(T_DATA, "id", 1000, "X SYMOBOL list")
#' # AA$new(envir = T_DATA, variable = "id", value = 1000, info = "hacd")
#' #
#' # # list variable
#' # AA$id
#' #
#' # # delete variable
#' # AA$delete(XENADB, "phen")
Class <- function(class) {
    Class <- function() {
        # # 结构化保存项目数据（重要数据）
        # # 用来存储项目的各种重要中间数据并给与标记信息
        # # 结构化保存项目数据（重要数据），新增属性如果和之前的属性名相同，则覆盖之前数据
        # 用法
        # T_DATA <- Class()
        # T_DATA$catinfo()
        # T_DATA$new(T_DATA, "id", 1000, "X SYMOBOL list")
        # T_DATA$new(envir = T_DATA, variable = "id", value = 1000, info = "hacd")
        # T_DATA$id
        privateLZ <- new.env(parent = emptyenv())
        # “属性” 初始化  ---------------
        # 基础属性1: info  初始化info信息
        assign(x = "info", value = "参数说明: ", envir = privateLZ)
        # “方法” 初始化  ---------------
        # 基础方法1：catinfo()  显示info信息
        `privateLZ`$catinfo <- function() {
            cat(get("info", envir = privateLZ), "\n")
        }
        # 基础方法2：new()       增加属性
        `privateLZ`$new <- function(envir, variable, value, info=NA) {
            assign(x = variable, value = value, envir = envir)
            demo <- paste0(get("info", envir = envir), "\n", variable, ": ", info)
            demo_check <- function(info) {
                # 检查增加的属性是否同名，如果同名则抛弃该属性之前的info信息
                info_split <- rev(strsplit(info, "\n")[[1]])
                info_split_2 <- sapply(strsplit(info_split, ": "), function(x) x[1])
                info_split <- info_split[!duplicated(info_split_2)]
                info_split_n <- paste(rev(info_split), collapse = "\n")
                return(info_split_n)
            }
            demo <- demo_check(demo)
            cat(demo, "\n")
            assign(x = "info", demo, envir = envir)
        }

        # 基础方法2：delete()       删除属性
        `privateLZ`$delete <- function(envir, variable) {
            if (exists(variable, envir = envir)) {
                rm(list = variable, envir = envir)
                cat(paste0("DELETE. Variable \"", variable, "\" deleted from the environment ",
                           as.character(substitute(envir)), ".\n"))
                # DELETE INFO --------------
                info_split <- strsplit(envir$info, "\n")[[1]]
                info_split_2 <- sapply(strsplit(info_split, ": "), function(x) x[1])
                delete.n <- which(!info_split_2[2:length(info_split_2)] %in% ls(envir)[1:16]) + 1
                info_split <- info_split[-delete.n]
                info_split_n <- paste(info_split, collapse = "\n")
                assign(x = "info", info_split_n, envir = envir)
            } else {
                cat(paste0("NOT FOUND. Variable \"", variable, "\" not found in the environment ",
                           as.character(substitute(envir)), ".\n"))
            }
        }

        return(privateLZ)
    }
    return(structure(Class(), class = class))
}


#' Add a attribute to an environment
#' @description Add a attribute to an environment, in this package, it's usually used to add a attribute/variable to the DataClass Object(environment)
#' @param envir object the DataClass name(environment name)
#' @param variable character new attribute name
#' @param value object new attribute value
#' @param info character the information of new attribute
#'
#' @return environment's new attribute
#' @export
#'
#' @examples # lznew(envir = test, variable = "id", value = 1000, info = "Hellow")
#' @author Jiang
lznew <- function(envir, variable, value, info=NA) {
    # 旧版本的分离式函数
    # [lznew已经被内置进Class函数中，并重名名为new]
    # 初始化检查，初始化catinfo()函数
    # if (length(strsplit(`envir`$info, "\n")[[1]]) <= 1) {
    #   `envir`$catinfo <- function() {
    #     cat(get("info", envir = envir))
    #   }
    # }
    assign(x = variable, value = value, envir = envir)
    demo <- paste0(get("info", envir = envir), "\n", variable, ": ", info)
    demo_check <- function(info) {
        # 检查增加的属性是否同名，如果同名则抛弃该属性之前的info信息
        info_split <- rev(strsplit(info, "\n")[[1]])
        info_split_2 <- sapply(strsplit(info_split, ": "), function(x) x[1])
        info_split <- info_split[!duplicated(info_split_2)]
        info_split_n <- paste(rev(info_split), collapse = "\n")
        return(info_split_n)
    }
    demo <- demo_check(demo)
    cat(demo, "\n")
    assign(x = "info", demo, envir = envir)
}


#' Delete a attribute to an environment
#' @description Delete a attribute and its information in an environment
#' @param envir object the DataClass name(environment name)
#' @param variable character the attribute name which you want to delete
#'
#' @return Null
#' @export
#'
#' @examples # lzdelete(envir = test, variable = "id")
#' @author Jiang
lzdelete <- function(envir, variable) {
    if (exists(variable, envir = envir)) {
        rm(list = variable, envir = envir)
        cat(paste0("DELETE. Variable \"", variable, "\" deleted from the environment ",
                   as.character(substitute(envir)), ".\n"))
        # DELETE INFO --------------
        info_split <- strsplit(envir$info, "\n")[[1]]
        info_split_2 <- sapply(strsplit(info_split, ": "), function(x) x[1])
        delete.n <- which(!info_split_2[2:length(info_split_2)] %in% ls(envir)[1:16]) + 1
        info_split <- info_split[-delete.n]
        info_split_n <- paste(info_split, collapse = "\n")
        assign(x = "info", info_split_n, envir = envir)
    } else {
        cat(paste0("NOT FOUND. Variable \"", variable, "\" not found in the environment ",
                   as.character(substitute(envir)), ".\n"))
    }
}
