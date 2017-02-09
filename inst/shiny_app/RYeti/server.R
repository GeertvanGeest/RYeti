library(shiny)
library(RYeti)

shinyServer(function(input, output) {
  spd <- reactive({
    inFile <- input$file1$datapath

    if (is.null(inFile))
      return(NULL)
    try_read_specdat <- try(
      speclist <- RYeti::read_specdat(inFile, sep = input$separator, dec = input$decimal),
      silent = TRUE)
    if(class(try_read_specdat) == "try-error"){
      validate(need(
        try_read_specdat,
        message = "Non-accepted input format. Check the separator and decimal signs."))
    }
    return(speclist)

  })

  output$measurementInput <- renderUI({
    inFile <- input$file1$datapath
    if (is.null(inFile))
      return(NULL)
    spd <- spd()

    selectInput('measurement', 'Select measurement to plot',
                choices = colnames(spd$spectraldata))
  })

  output$spectraldata <- renderTable({
    spd <- spd()
    return(spd$spectraldata)
  }, digits = 4, rownames = TRUE)

  output$metadata <- renderTable({
    spd <- spd()
    return(spd$metadata)
  }, rownames = TRUE, digits = 3)

  output$spectralplot <- renderPlot({
    inFile <- input$file1$datapath
    if(is.null(inFile)){
      return(NULL)
    }
    spd <- spd()
    specdat <- spd$spectraldata[,input$measurement]
    if(length(specdat) == nrow(spd$spectraldata)){
      names(specdat) <- rownames(spd$spectraldata)
      pss <- calc_pss(specdat, absorbing_states)
      par(mar=c(5,5,4,2)+0.1)
      plot_spectra(specdat, ylim = c(0,1.1*max(specdat)),
                   cex.lab = 1.4,
                   cex.axis = 1.4,
                   xlab = "Wavelength (nm)",
                   ylab = expression(paste("E"['e'], "(W m"^'-1',")")),
                   main = "Measured spectrum")
      text(x = 0.9*max(as.numeric(names(specdat))),
           y = max(specdat),
           labels = paste0("PSS = ", round(pss, 3)),
           cex = 1.3)
    } else {
      return(NULL)
    }
  })

  output$PSS_plot <- renderPlot({
    inFile <- input$file1$datapath
    if(is.null(inFile)){
      return(NULL)
    }
    spd <- spd()
    specdat <- spd$spectraldata[,input$measurement]
    if(length(specdat) == nrow(spd$spectraldata)){
      data("absorbing_states")
      absorbing_states <- absorbing_states[rownames(spd$spectraldata),]
      wl <- as.integer(rownames(absorbing_states))
      par(mar=c(5,5,4,2)+0.1)
      plot(wl, absorbing_states$sigma_r, col = "red", type = "o", pch = 19,
           cex.lab = 1.4,
           cex.axis = 1.4,
           xlab = "Wavelength (nm)",
           ylab = "Photochemical cross-section",
           main = "Absorbing states")
      lines(wl, absorbing_states$sigma_fr, col = "darkred", type = "o", pch = 19)
      legend("topright", bty = "n", pch = 19, col = c("red", "darkred"), legend = c(expression('P'['r']),
                                                                                    expression('P'['fr'])),
             cex = 1.4)
    } else {
      return(NULL)
    }
  })


})

