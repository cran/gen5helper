#' this is a file for functions that are universally useful at common data manipulations

#' Convert factor to numeric
#'
#' @param x factor
#'
#' @export
#' @examples
#' factor2num(factor(c('1', '10', '100')))
#'
factor2num <- function(x){as.numeric(as.character(x))}

#' Map the unique values of a vector
#'
#' @param x factor or character
#' @param facs character. It maps unique(x) to facs
#' @param bNaturalSort binary. Whether to convert factor in natural order.
#'
#' @return factor
#' @importFrom plyr mapvalues
#' @importFrom naturalsort naturalfactor
#' @export
#' @examples
#' mapvalues_(c("A","A","B","C"), c("one", "two", "three"))
#' mapvalues_(c("apple", "apple", "banana", "pineable", "pineable"), c(3, 2, 1), bNaturalSort = TRUE)
#'
mapvalues_ <- function(x, facs, bNaturalSort = FALSE) {
    #NULLing
    if (bNaturalSort) {
        naturalsort::naturalfactor(plyr::mapvalues(x, unique(x), facs))
    }else{
        factor(plyr::mapvalues(x, unique(x), facs), levels = unique(facs))
    }
}

#' Cast an object to match class of another object
#'
#' @param x object to transform
#' @param vec object to extract class
#'
#' @export
#' @examples
#' as.is(c("1", "2", "3"), 1:3)
#'
as.is <- function(x, vec) {
    if(class(vec) == 'numeric') {
        return(as.numeric(as.character(x)))
    } else if (class(vec) == 'character') {
        return(as.character(x))
    } else {
        return(x)
    }
}

#' Smooth a vector using moving average
#'
#' @param vec numeric vector
#' @param naverage width of moving average
#'
#' @return smoothed
#' @export
#' @examples
#' smooth.mean(1:10, 2)
#' smooth.mean(1:10, 3)
#' smooth.mean(1:10, 5)
smooth.mean <- function(vec, naverage){
    stopifnot(naverage <= as.integer(naverage), 0 < naverage)
    nvec <- c()
    for(i in 1:length(vec)) {
        if(i %% naverage == 0) {
            nvec[(i-naverage+1):i] <- mean(vec[(i-naverage+1):i])
        }
    }
    i <- length(vec)
    ni <- length(nvec)
    if(ni < i){
        nvec[(ni+1):i] <- mean(vec[(ni+1):i])
    }
    return(nvec)
}

#' Return range of a vector
#'
#' This computes the range of a vector as a value.
#'
#' @param x numeric
#' @param na.rm bool whether to remove NA values.
#'
#' @return numeric value
#' @export
#' @examples
#' range_(c(1, 5, 10))
#' range_(c(1, 5, 10, NA), na.rm = TRUE)
range_ <- function(x, na.rm=TRUE){
    max(x, na.rm = na.rm)-min(x, na.rm = na.rm)
}

#' Scale a vector to 0-1 by min and max
#'
#' @param x numeric
#' @param na.rm bool whether to remove NA values.
#'
#' @return a normalized vector
#' @export
#' @examples
#' normalize(0:10)
#' normalize(c(1, 100, NA, 10), na.rm = TRUE)
normalize <- function(x, na.rm = TRUE){
    (x-min(x, na.rm = na.rm))/range_(x, na.rm = na.rm)
}

#' Run ungroup() and as.data.frame()
#'
#' @param .data grouped data.frame
#'
#' @return an ungrouped data.frame()
#' @importFrom dplyr ungroup
#' @export
#' @examples
#' \donttest{
#' data <- data.frame(m=c(1,2), n=c(2,3), group=c('a','b'))
#' data %>%
#'     group_by(group) %>%
#'     ungroup_()
#' }
ungroup_ <- function(.data){
    return(as.data.frame(ungroup(.data)))
}

#' write.csv without row.names
#'
#' This function returns the object passed in, can be used for dplyr pipeline.
#'
#' @param x object
#' @param file filename for write.csv
#'
#' @return whatever object passed in
#' @importFrom utils write.csv
#' @export
#' @examples
#' \donttest{
#' write.csv_(data.frame(a=1, b=2, c=3), file.path(tempdir(), "data.csv"))
#'
#' data <- data.frame(a=1, b=2, c=3)
#' data %>%
#'     file.path(tempdir(), "data.csv") %>%
#'     print()
#' }
write.csv_ <- function(x, file){
    write.csv(x, file, row.names = FALSE)
    return(x)
}

#' saveRDS and return .data
#'
#' This function returns the object passed in, can be used for dplyr pipeline.
#'
#' @param .data object to be saved
#' @param file filename to save
#' @param ... for saveRDS
#'
#' @return whatever object passed in.
#' @export
#' @examples
#' \donttest{
#' data <- data.frame(a=1, b=2, c=3)
#' data %>%
#'     saveRDS_(file.path(tempdir(), "data.rds")) %>%
#'     print()
#' }
#'
saveRDS_ <- function(.data, file, ...){
    saveRDS(object = .data, file = file, ...)
    return(.data)
}

#' Return most frequent numbers
#'
#' get the n most frequent elements in an array
#'
#' @param x an array of elements
#' @param n integer, default is 1
#'
#' @return the most n elements
#' @export
#' @examples
#' most.freq(c('a', 'a', 'b', 'b', 'b', 'c'), n = 2)
#' most.freq(c(1, 1, 2, 3, 3, 3, 4, 4), n = 2)
#'
most.freq <- function(x, n = 1){
    if(length(x) == 1) return(x)
    type <- typeof(x)
    sort.freq <- sort(table(x),decreasing=TRUE)
    this.freq <- sort.freq[n]
    this.index <- which(sort.freq < this.freq) - 1
    if(length(this.index) == 0)
        this.index <- length(sort.freq)
    results <- names(sort.freq[1:this.index])
    class(results) <- typeof(x)
    return(results)
}
