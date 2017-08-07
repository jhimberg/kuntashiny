navbarPage("Kuntatilastoja",
           tabPanel("Kartta",
           pageWithSidebar(
             headerPanel('Kuntatilastoja'),
             sidebarPanel(
               tags$style(type="text/css", "#kartta.recalculating { opacity: 1.0; }"),
               selectInput('muuttuja', 'Muuttuja', kunta.stat.vars),
               sliderInput('vuosi', "Vuosi",
                           min=min(vuodet),
                           max=max(vuodet),
                           value=min(vuodet),
                           sep="", step=1, animate = T),
               radioButtons("karttatyyppi", label = "Karttatyyppi",
                            choices = karttatyyppi$label, selected=karttatyyppi$label[1]),
               wellPanel("Kartogrammi painottaa kunnan pinta-alaa vuoden 2016 asukasmäärällä")
             ),
             mainPanel(
               ggiraphOutput("kartta", height="1200px"),
               "Data: Tilastokeskus, https://pxnet2.stat.fi/PXWeb/Resources/PX/Databases/Kuntien_avainluvut/2017/kuntien_avainluvut_2017_aikasarja.px"
             )
           )),
           tabPanel("GIF-animaatio",
                    pageWithSidebar(
                      headerPanel('Generoi GIF-video'),
                      sidebarPanel(
                        "Animaation käynnistyminen kestää n. 30 sekuntia!",
                        selectInput('muuttujavideo', 'Muuttuja', kunta.stat.vars),
                                               radioButtons("karttatyyppivideo", label = "Karttatyyppi",
                                     choices = karttatyyppi$label, selected=karttatyyppi$label[1]),
                        wellPanel("Kartogrammi painottaa kunnan pinta-alaa vuoden 2016 asukasmäärällä")
                      ),
                      mainPanel(
                        plotOutput("karttavideo", height="1200px")
                      )
                    )
           )
)
           