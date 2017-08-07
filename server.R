
function(input, output, session) {
  
  observe({
    updateSliderInput(session, "vuosi", 
                      value=min(kuntadata$vuosi[!is.na(kuntadata[input$muuttuja])]),
                      min=min(kuntadata$vuosi[!is.na(kuntadata[input$muuttuja])]),
                      max=max(kuntadata$vuosi[!is.na(kuntadata[input$muuttuja])]) )
    
  })
  
  output$karttavideo <- renderImage({
    
    outfile <- tempfile(fileext='.gif')
    
    p <- kartta.animaatio(karttaGIFData(), title.label=input$muuttujavideo, 
                aluejako=plyr::mapvalues(input$karttatyyppivideo, 
                                         karttatyyppi$label, 
                                         karttatyyppi$aluejako, warn_missing = F))
    ani.options(interval=0.5, ani.width=500, ani.height=650)
    gganimate(p, file="outfile.gif")
    
    # Return a list containing the filename
    
    list(src = "outfile.gif",
         contentType = 'image/gif',
         height="800px",
         alt = "Tää on rikki"
    )}, deleteFile = TRUE)
  
  karttaData<-reactive({g <- kuntadata[c("vuosi", "kuntanimi", input$muuttuja)]
  names(g) <- c("vuosi","alue","x")
  color.limits<-c(min(g["x"], na.rm=TRUE), max(g["x"],na.rm=TRUE))
  g <- filter(g, !is.na(x) & vuosi == input$vuosi) 
  p <- kartta(select(g, x, alue), 
              title.label = paste(input$muuttuja,input$vuosi, sep=", "),
              color.limits=color.limits, 
              aluejako = plyr::mapvalues(input$karttatyyppi, 
                                         karttatyyppi$label, 
                                         karttatyyppi$aluejako, warn_missing = F))})
  
  karttaGIFData<-reactive({g <- kuntadata[c("vuosi", "kuntanimi", input$muuttuja)]
  g <- kuntadata[c("vuosi", "kuntanimi", input$muuttujavideo)]
  names(g) <- c("aika","alue","x")
  g <- filter(g, !is.na(x)) 
  })
  
  output$kartta <- renderggiraph({
    ggiraph(code={print(karttaData())}, 
            tooltip_extra_css="background-color:white", tooltip_opacity=.9)
    })
  }
