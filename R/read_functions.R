read_spectro <- function(file, read_tag = "Readings", meas_tag = "Measurement"){
    dat <- read_xml(file)
    readings <- xml_find_first(dat, read_tag)
    meas <- xml_attrs(xml_find_all(readings, meas_tag))
    meas <- lapply(meas, function(x){as.data.frame(t(x), stringsAsFactors=F)})
    meas <- bind_rows(meas)
    meas$WL <- as.numeric(meas$WL)
    meas$Tran <- as.numeric(meas$Tran)
    meas$Abs <- as.numeric(meas$Abs)
    return(meas)
}
