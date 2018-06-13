
function(input, output, session) {
  
  output$aikasarja <-renderPlot({
    g <- koko.maa[, c("vuosi", input$muuttuja)]
    names(g)[2] <- "x" 
    g <- g[!is.na(g$x),]
    
    h <- kuntadata[kuntadata$kuntanimi %in% input$kartta_selected, c("kuntanimi", input$muuttuja, "vuosi")]
    names(h)[2] <- "x" 
    
    color.limits <- c(min(c(kuntadata[, input$muuttuja], g$x), na.rm=TRUE), 
                      max(c(kuntadata[, input$muuttuja],g$x), na.rm=TRUE))
    vuosi.limits <- c(min(g$vuosi, na.rm=TRUE), 
                      max(g$vuosi, na.rm=TRUE))
    p <- ggplot(g, aes(x=vuosi,y=x)) + 
      geom_line(color="dodgerblue3",size=2.5) +
      coord_cartesian(xlim=vuosi.limits, ylim=color.limits) +
      ggtitle("Koko maan keskiarvo: vahvennettu sininen käyrä") +
      ylab(input$muuttuja)
    p <- p + geom_line(data=h, aes(x=vuosi, y=x, group=kuntanimi, color=kuntanimi))
    p
  })
  
  observe({
    updateSliderInput(session, 
                      "vuosi", 
                      value=min(kuntadata$vuosi[!is.na(kuntadata[input$muuttuja])]),
                      min=min(kuntadata$vuosi[!is.na(kuntadata[input$muuttuja])]),
                      max=max(kuntadata$vuosi[!is.na(kuntadata[input$muuttuja])]) )
    
  })
  
  karttaData <- reactive({g <- kuntadata[c("vuosi", "kuntanimi", input$muuttuja)]
  names(g) <- c("vuosi", "alue", "x")
  color.limits <- c(min(g$x, na.rm=TRUE), max(g$x, na.rm=TRUE))
  g <- filter(g, !is.na(x) & vuosi == input$vuosi) 
  p <- kartta(select(g, x, alue), 
              title.label = paste(input$muuttuja, input$vuosi, sep=", "),
              color.limits=color.limits, 
              aluejako = plyr::mapvalues(input$karttatyyppi, 
                                         karttatyyppi$label, 
                                         karttatyyppi$aluejako, warn_missing = F))})
  

  output$kartta <- renderggiraph({
    ggiraph(code={print(karttaData())}, 
            tooltip_extra_css="background-color:white", 
            tooltip_opacity=.9,
            selection_type = "multiple"
            )
    })
  }
