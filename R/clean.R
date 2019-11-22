#' Clean Gen5 exported data
#'
#' g5h.clean2() returns technically correct data.frame from Gen5 2.06 exported
#' tab-delim data. The exported data can be generated using default export
#' protocol in Gen5 2.06. See Gen5 User Guide for more information.
#'
#' @param files a vector of names of the file which the data are to be read from.
#' If it does not contain an absolute path, the file name is relative to the
#' current working directory, getwd().
#'
#' @return technically correct data.frame.
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \donttest{
#' # suppose "exported_data_1.txt" and "exported_data_2.txt" are the exports from Gen5 2.06
#'
#' # this line will clean one exported data
#' data <- g5h.clean2("exported_data_1.txt")
#'
#' # this line will clean two exported data and return one appended dataset
#' data <- g5h.clean2(c("exported_data_1.txt", "exported_data_2.txt"))
#' }
#'
g5h.clean2 <- function(files) {
    bind_rows(lapply(files, function(file) g5h.clean_(file)))
}

#' Clean Gen5 exported data
#'
#' g5h.clean_() returns technically correct data.frame from Gen5 2.06 exported
#' tab-delim data. The exported data can be generated using default export
#' protocol in Gen5 2.06. See Gen5 User Guide for more information.
#'
#' @param file the name of the file which the data are to be read from. If it
#' does not contain an absolute path, the file name is relative to the current
#' working directory, getwd().
#'
#' @return technically correct data.frame.
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom stats sd lm
g5h.clean_ <- function(file) {
    #NULLing to appease R CMD CHECK
    val <- well <- Time <- time_hour <- time_min <- time_sec <- total_min <-
        time.start <- time.min <- realTime <- readingType <- temp <-
        time <- realHour <- group <- NULL
    strsplit_ <- function(strVec, split, n = 1){
        sapply(strVec, function(x){
            strsplit(x, split)[[1]][n]
        }, simplify = T)
    }
    read2ds <- function(file, start.row, end.row) {
        # transfrom readLines output to data.frame
        ds <- read.csv(file, header = T, nrows = end.row - start.row,
                       skip = start.row - 1, sep = '\t', stringsAsFactors = F)
        ds2 <- tidyr::gather(ds, 'well', 'val', 3:ncol(ds)) %>%
            filter(!is.na(val)) %>%
            tidyr::separate(well, c('row', 'col'), sep = 1, remove = F)
        names(ds2)[2] <- 'temp'
        # filter out masked wells
        maskedRowCol <- ds2 %>%
            filter(grepl('\\*', val)) %>%
            select(row, col)
        ds3 <- ds2 %>%
            filter(!((row %in% unique(maskedRowCol$row)) &
                         (col %in% unique(maskedRowCol$col)))) %>%
            mutate(val = as.numeric(val)) %>%
            mutate(time_hour = as.numeric(strsplit_(Time, ':', 1))) %>%
            mutate(time_min = as.numeric(strsplit_(Time, ':', 2))) %>%
            mutate(time_sec = as.numeric(strsplit_(Time, ':', 3))) %>%
            mutate(total_min = time_hour * 60 + time_min + time_sec/60) %>%
            mutate(time.min = total_min - total_min[1]) %>%
            select(-time_hour, -time_min, -time_sec, -total_min)
        }
    ds.raw <- readLines(file)
    line.date.times <- which(grepl('Date\t', ds.raw))
    line.procedures <- which(grepl('Procedure Details', ds.raw))
    line.reads <- line.procedures - 1 +
        grep('Start Kinetic',
             ds.raw[line.procedures[1]:(line.procedures[1] + 20)])
    if (length(line.reads) == 0) {
        warning('Input Format Not Defined')
        return(NULL)
    }
    out <- bind_rows(lapply(1:length(line.reads), function(read.i){
        line.read <- line.reads[read.i]
        line.date.time <- line.date.times[read.i]
        time.start.str <- strptime(
            paste(strsplit(ds.raw[line.date.time],'\t')[[1]][2],
                  strsplit(ds.raw[line.date.time+1], '\t')[[1]][2]),
            format = '%m/%d/%Y %I:%M:%S %p',
            tz = '')
        l <- strsplit(ds.raw[line.reads[read.i]], ' ')[[1]]
        cur.total.reads <- as.integer(l[which(l == 'Reads') - 1])
        cur.limt <- c(line.reads, length(ds.raw)+1)[c(read.i, read.i+1)]
        cur.range <- cur.limt[1]:(cur.limt[2]-1)
        cur.ds <- ds.raw[cur.range]
        readingTypes <- sapply(which(grepl('Time\tT', cur.ds)),
                               function(x){
                                   print(cur.ds[x-2]);
                                   strsplit(cur.ds[x-2], '\t')[[1]][1]
                                   })
        bind_rows(lapply(1:length(readingTypes), function(i){
            start.row <- cur.limt[1] +
                which(grepl('Time\tT', cur.ds))[i] - 1
            end.row   <- start.row + cur.total.reads
            read2ds(file, start.row, end.row) %>%
                mutate(readingType = readingTypes[i])
            })) %>%
            mutate(time.start = as.POSIXct(time.start.str)) %>%
            mutate(realTime = time.start + time.min * 60) %>%
            select(realTime, well, row, col, readingType, val, temp)
    }))
    out <- out %>% filter(!is.na(realTime), !is.na(val))
    return(out)
}
