
<!-- README.md is generated from README.Rmd. Please edit that file -->
relectionutils
==============

This package has a few functions that I use regularly to process election data, including:

-   **elec\_find\_winner** -- For a data frame with city/precinct/place results by row, each candidate/issue raw vote total in a separate column. This function finds the candidate/issue/column name with the highest total in each row, returning the column name. It accounts for ties.

-   **elec\_pcts\_by\_row** -- Also for a data frame with city/precinct/place results by row, each candidate/issue raw vote total by column. This function calculates the percent for each candidate/issue in each row and adds intuitively named percent columns to the data frame.
