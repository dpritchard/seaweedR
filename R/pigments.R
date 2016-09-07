ritchie_2008 <- function(ws){
    needed_wl <- c(632, 649, 665, 696)
    indx <- match(needed_wl, ws$WL)
    for(a in needed_wl){
        assign(paste0("A", a), ws[ws$WL==a, "Abs"])
    }
    Chla <- as.numeric(0.0604*A632-4.5224*A649+13.2969*A665-1.7453*A696)
    Chlb <- as.numeric(-4.1982*A632+25.7205*A649-7.4096*A665-2.7418*A696)
    Chlc <- as.numeric(28.4593*A632-9.9944*A649-1.9344*A665-1.8093*A696)
    Chld <- as.numeric(-0.2007*A632+0.0848*A649-0.1909*A665+12.1302*A696)
    ChlT <- as.numeric(24.1209*A632+ 11.2884*A649+3.762*A665+5.8338*A696)
    out <- list()
    out[["data"]] <- ws[indx, ]
    out[["conc"]] <- data.frame("Chla"=Chla, "Chlb"=Chlb, "Chlc"=Chlc,
                      "Chld"=Chld, "TotalChl"=ChlT)
    return(out)
}
beer_eshel_1985 <- function(ws){
    needed_wl <- c(455, 564, 592, 618, 645, 730)
    indx <- match(needed_wl, ws$WL)
    for(a in needed_wl){
        assign(paste0("A", a), ws[ws$WL==a, "Abs"])
    }
    PE <- as.numeric(((A564-A592)-(A455-A592)*0.20)*0.12)
    PC <- as.numeric(((A618-A645)-(A592-A645)*0.51)*0.15)
    out <- list()
    out[["data"]] <- ws[indx, ]
    out[["conc"]] <- data.frame("PE"=PE, "PC"=PC)
    return(out)
}
