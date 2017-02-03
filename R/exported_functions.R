#' Calculate PSS
#'
#' @param f A named vector of signal intensities per wavelength.
#' @param abs Absorbance values of Pr and Pfr.
#'
#' @return
#' A PSS value
#' @export
#'
#' @examples
calc_pss = function(f, abs){
  ints <- intersect(names(f), rownames(abs))
  f_int <- f[ints]
  abs_int <- abs[ints,]
  sigma_r_cum = sum(f_int*abs_int$sigma_r)
  sigma_fr_cum = sum(f_int*abs_int$sigma_fr)
  pss = sigma_r_cum/(sigma_r_cum+sigma_fr_cum)
  return(pss)
}

#' Read spectral data from Yeti
#'
#' @param file A file name.
#'
#' @return A list containing metadata and spectral data.
#' @export
#'
#' @examples
read_specdat <- function(file){
  ex <- readLines(file)
  emptyline <- paste(rep(";", nchar(ex[1])), collapse = "")
  emptylines <- which(grepl(pattern = emptyline, x = ex))
  start_meta <- which(grepl(pattern = "Name", x = ex))[1]
  end_meta <- emptylines[emptylines > start_meta][2]

  #end_meta <- which(grepl(pattern = "DWl [nm]", x = ex, fixed = TRUE))[1]
  meta <- read.csv2(file = file, skip = start_meta - 1, nrows = end_meta - start_meta-1,
                    row.names = 1,
                    stringsAsFactors = FALSE)
  meta <- t(as.matrix(meta))
  meta <- gsub(",", ".", meta)
  meta <- data.frame(meta, stringsAsFactors = FALSE)

  start_par <- which(grepl(pattern = "PAR", x = ex))[1]
  if(is.na(start_par)){
    start_par <- which(grepl(pattern = "Photosynthetically Active Radiation", x = ex, fixed = TRUE))[1]
  }

  end_par <- emptylines[emptylines > start_par][1]
  par <- read.csv2(file = file, skip = start_par, nrows = end_par - start_par -1,
                    row.names = 1, stringsAsFactors = FALSE, header = F)
  par <- as.data.frame(t(as.matrix(par)))
  meta <- cbind(meta, par)

  colname_int_time <- "T_int..ms."
  if(!colname_int_time %in% colnames(meta)){
    colname_int_time <- "Integration.Time..ms."
  }

  Ee_pos <- grepl("Ee..W.sqm", colnames(meta), fixed = TRUE)
  if(!any(Ee_pos)){
    Ee_pos <- grepl("Irradiance..W.sqm", colnames(meta), fixed = TRUE)
  }
  colname_Ee <- colnames(meta)[Ee_pos]

  colname_PAR <- "Ephot (Begin..End) [umol/s sqm]"
  if(!colname_PAR %in% colnames(meta)){
    colname_PAR <- "Ephot (Begin..End) [uMol/s sqm]"
  }

  meta <- data.frame(Correction = meta$Correction, Date = meta$Date,
                     Time = meta$Time, Integration_time = as.numeric(meta[,colname_int_time]),
                     Ee = as.numeric(meta[,colname_Ee]),
                     PAR = meta[,colname_PAR],
                     stringsAsFactors = FALSE)
  rownames(meta) <- paste0("measurement", 1:nrow(meta))

  start_specdat <- which(grepl(pattern = "^Wavelength \\[nm\\]", x = ex))[1]
  #start_specdat <- tail(start_specdat, 1)

  end_specdat <- emptylines[emptylines > start_specdat][1]
  specdat <- read.csv2(file = file, skip = start_specdat - 1, nrows = end_specdat - start_specdat - 1,
                       row.names = 1)
  colnames(specdat) <- paste0("measurement", 1:(ncol(specdat)))
  meta$PSS <- apply(X = specdat, MARGIN = 2, function(x){
    names(x) <- rownames(specdat)
    calc_pss(x, absorbing_states)
  })

  speclist <- list(spectraldata = specdat, metadata = meta)
  return(speclist)
}


#' Create false color from wavelength
#'
#' @param wl Interger. Wavelength.
#'
#' @return
#' A hexadecimal RGB value.
#' @export
#'
#' @examples
wavelengthRGB <- function(wl){
  gamma <-0.8
  IntensityMax <- 255

  adjust <- function(colorIntensity, fac, Intensitymax = 255, gamma = 0.8){
    if(colorIntensity == 0){
      return(0)
    } else {
      return(round(IntensityMax*(colorIntensity*fac)^gamma))
    }
  }

  factor <- 0
  if(wl >= 380 & wl <= 439){
    red <- -(wl - 440) / (440 - 380)
    green <- 0
    blue <- 1
    if(wl <= 419){
      factor <- 0.3 + 0.7*(wl - 380) / (420 - 380)
    }
  } else if(wl >= 440 & wl <= 489){
    red <- 0
    green <- (wl - 440) / (490 - 440)
    blue <- 1

  } else if(wl >= 490 & wl <= 509){
    red <- 0
    green <- 1
    blue <- -(wl - 510) / (510 - 490)
  } else if(wl >= 510 & wl <= 579){
    red <- (wl - 510) / (580 - 510)
    green <- 1
    blue <- 0
  } else if(wl >= 580 & wl <= 644){
    red <- 1
    green <- -(wl - 645) / (645 - 580)
    blue <- 0
  } else if(wl >= 645 & wl <= 780){
    red <- 1
    green <- 0
    blue <- 0
    if(wl >= 700){
      factor <- 0.3 + 0.7*(780 - wl) / (780 - 700)
    }
  } else {
    red <- 0
    green <- 0
    blue <- 0
  }
  if(wl >=420 & wl < 700){
    factor <- 1
  }

  r <- adjust(colorIntensity = red, fac = factor)
  g <- adjust(colorIntensity = green, fac = factor)
  b <- adjust(colorIntensity = blue, fac = factor)
  return(rgb(r,g,b, maxColorValue = 255))
}

#' Plot spectrum
#'
#' @param f Named vector of intensity
#' @param ... Arguments passed to plot
#'
#' @return
#' @export
#'
#' @examples
plot_spectra <- function(f, xlab = "wavelength", ylab = "Ee (W/(m^2*nm))", ...){
  wl <- as.integer(names(f))
  cols <- sapply(wl, wavelengthRGB)
  plot(wl, f, col = cols, type = "o", pch = 19,
       xlab = xlab, ylab = ylab, ...)
}

