
<!-- README.md is generated from README.Rmd. Please edit that file -->
relectionutils
==============

This package has a few functions that I use regularly to process election data:

-   **elec\_find\_winner** -- For a data frame with city/precinct/place results by row, each candidate/issue raw vote total in a separate column. This function finds the candidate/issue/column name with the highest total in each row, returning the column name. It accounts for ties.

-   **elec\_pcts\_by\_row** -- Also for a data frame with city/precinct/place results by row, each candidate/issue raw vote total by column. This function calculates the percent for each candidate/issue in each row and adds intuitively named percent columns to the data frame.

IN PROGRESS:

-   **elec\_results\_map** -- Creates an interactive Leaflet map of results for a two-candidate race or yes/no ballot question, one color for each option. This map shows *color intensity by voting percent* and not simply binary colors showing who won and lost. There is an optional layer for turnout if the data includes a column named Turnout with turnout percentage as a decimal value between 0 and 1.
