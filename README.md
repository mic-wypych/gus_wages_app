This is a simple shiny app that allows to view annual mean wages on powiat (county) level in Poland.

The data for wages come from GUS Bank Lokalny and the geographical data on counties from GIS support Bank wiedzy.

So far the app has a map of all counties with their annual mean wages and a table that shows changes in wages from 2002 to 2023

Issues & TO DOs:
- app startup is extremely slow. There is something wrong with the ggiraph and geom_sF_interactive that makes it very slow.
- Add year-level summaries to the sidepanel
- a few counties get duplicated for some weird reason and replace other counties. E.g. Powiat Tomaszowski.
