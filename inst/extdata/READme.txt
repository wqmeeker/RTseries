The data sets in this folder have one of two different formats.

Most univariate time series data sets (just a single variable) are given
as text (.txt suffix) files. The time sequence (and order of reading)
is left to right, one line at a time.  For example, there might be
one line of numbers for each year. These text files do not contain any
explicit information about dates or variable names. That information is usually
provided elsewhere (e.g., in an assignment or example RTseries commands).
These file are read into R using a scan command and the result is a vector.

Data sets that have more than one variable are given a comma-separated (.csv) files.
For example, the file Airline.csv has variables for the response, time, and month.  These files
are read into R using the read.csv function and the result is a data frame.

More information and examples are given in the RTseries vignette.

