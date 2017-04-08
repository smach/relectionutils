#' elec_results_map
#' STILL IN PROGRESS! This function takes a vote results file that has been run through the elec_find_winner and elec_pcts_by_row functions and combined with a GIS shapefile. The result is a Leaflet map with diverging colors for the winning and losing candidates/positions, and an optional turnout layer. Vignette to come.
#'
#' @param gisdatafile - sf (simple features) geospatial file combined with vote results data that was already processed with elec_find_winner and elec_pcts_by_row
#' @param winner - string name of winning candidate or position
#' @param loser - string name of losing candidate or position
#' @param placeCol - string name of column that holds place names. If you want names to display as "Precinct 1" you must have a placeCol column that combines the string "Precinct " with the precinct number.
#' @param wincolors - string name of ColorBrewer palette. Defaults to "Greens."
#' @param losecolors - string name of ColorBrewer palette. Defaults to "Reds."
#' @param turnout - logical Whether data includes a column named Turnout with turnout percents as a fraction
#' @param turnoutcolors - string name of ColorBrewer palette. Defaults to "Greens."
#' @param WinPctMargin - string name of column with the difference between the winning percent and losing percent for each place. May need to be calculated manually.
#'
#' @return leaflet object
#' @export
#'
elec_winners_map <- function(gisdatafile, winner = "Yes", loser = "No", placeCol = "Precinct", wincolors = "Greens", losecolors = "Reds", turnout = TRUE, turnoutcolors = "Greens", WinPctMargin = "YesPctMargin"){

  suppressPackageStartupMessages(library("leaflet"))
  vote_percent_range <- sort(unique(abs(gisdatafile[[WinPctMargin]])))

  winPalette <- colorNumeric(palette = wincolors, domain = vote_percent_range)
  losePalette <- colorNumeric(palette = losecolors, domain = vote_percent_range)
  turnoutPalette <- colorNumeric(palette = turnoutcolors, domain = CharterVote$Turnout)

  WinnerPctColName <- paste0(winner, ".pct")
  LoserPctColName <- paste0(loser, ".pct")

  winVote <- gisdatafile[gisdatafile$Winner == winner,]
  loseVote <- gisdatafile[gisdatafile$Winner == loser,]
  winPopup <- paste0("<b>", as.vector(winVote[[placeCol]]), "</b><br />", winner, ": ", winVote[[WinnerPctColName]], "%<br />", loser, ": ", winVote[[LoserPctColName]], "%<br /><br />")
  if(turnout){
    winPopup <- paste0(winPopup, "Turnout: ", scales::percent(winVote$Turnout))
  }

  losePopup <- paste0("<b>", as.vector(loseVote[[placeCol]]), "</b><br />", winner, ": ", loseVote[[WinnerPctColName]], "%<br />", loser, ": ", loseVote[[LoserPctColName]], "%<br /><br />")
  if(turnout){
    losePopup <- paste0(losePopup, "Turnout: ", scales::percent(loseVote$Turnout))
  }

  if(turnout){
    turnoutPopup <- paste0("<b>", as.vector(gisdatafile[[placeCol]]), "</b><br />Turnout: ", scales::percent(gisdatafile$Turnout))
  }

  # Create map
  results_map <- leaflet(gisdatafile) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      data = winVote,
      weight = .5,
      opacity = .5,
      color = "black",
      fillOpacity = .75,
      popup = winPopup,
      fillColor = ~winPalette(winVote[["YesPctMargin"]]),
      group = "Voting Results"
    ) %>%
    addPolygons(
      data = loseVote,
      weight = .5,
      opacity = .5,
      color = "black",
      fillOpacity = .75,
      popup = losePopup,
      fillColor = ~losePalette(abs(loseVote[["YesPctMargin"]])),
      group = "Voting Results"
    )

  if(turnout){
    results_map <- results_map %>%
      addPolygons(
        weight = .5,
        opacity = .5,
        color = "black",
        fillOpacity = .75,
        popup = turnoutPopup,
        fillColor = ~turnoutPalette(gisdatafile[["Turnout"]]),
        group = "Turnout"
      )  %>%
      addLayersControl(
        baseGroups = c("Voting Results", "Turnout"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)

      )

  }

  return(results_map)


}
