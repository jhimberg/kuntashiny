# kuntashiny
## Shiny-esimerkki kuntien avainluvuilla
* global.R, server.R ja ui.R ovat varsinainen serveri-frontage - koodi.
* initGeoDemografia.R käsittelee kartta- ja demografiadatat sopivaan muotoon (Data-kansiosta)
* Data sisältää valmiiksi haetut ja lasketut kartat ja kuntadatan. (Lisäksi mm. Paavo-datan ja muuta kuntadataa, jota ei tässä käytetä)
  * Data/createDemografiat.R: tällä on tehty tehdä Data-kansion sisältö - lukuunottamatta kartogrammia joka on tehty erikseen cartogram-paketilla - ei toimi enää, koska Tilastokeskuksen datojen jakaminen muuttunut

## Keskeiset kirjastot
* dplyr, tidyr: tidyversen datankäsittleyä
* ggplot2, ggiraphe: Tidyversen ggplot-visualisointi ja siihen interaktiivinen laajennos
