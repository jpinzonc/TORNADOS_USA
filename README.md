In North Texas, part of daily life are the storms, specially TORNADOS.

During Spring, these natural events become a conversation topic especially for those of us that did not grow around them. Since I move here, I have heard about tornados hitting "urban" areas more often than "rural" areas and how previous years had more, or less, of these spinning circles of wind. 

I have been curious about these and other tornado-related topics but have not had the chance to look at actual data. 

Late last year, I find a set of data on storm events compiled by NOAA's National Center for Environmental Information. These datasets included tornado records between 1951 and 2015. I decided to apply my skills to R and figure out if I can answer some of the questions I came along about tornados. 

In this script, I use dplyr, ggmap and tydir, among other libraries to download and clean the data from NOAA (http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/). In the cleaned data, I determine number of tornados since 1951 in the US and number and average tornados per year and per scale (F and EF). Then I use the deaths (DEATHS_DIRECT and DEATHS_INDIRECT) columns to determine number of deaths caused by tornados. In some of these steps I include commands to plot the results. Then, I use ggmap to plot the locations of tornados and those that caused deaths around a city of interest on a google map.  Finally, I compare the number of tornados between two locations. The file (Tornado_3.16.R) includes more comments for most of, if not all, the steps in the script. 

This project includes ideas, steps, and commands from several sources (sites and blogs), including:

http://www.guru-gis.net/batch-download-files-using-r/ 
https://mpiccirilli.github.io/  
https://github.com/mpiccirilli/USMassShootings 
http://gis.stackexchange.com/questions/119736/ggmap-create-circle-symbol-where-radius-represents-distance-miles-or-km 

It is possible to start from the beginning of the script downloading all files from NOAA (CHECK FOR ERRORS IN THE COORDINATES) or using the files that I have added here (tornados_10-16, and tornados_clean_10-16.csv). 

More details and graphs on 

https://blogrblog.wordpress.com/2016/10/13/tornados-and-r
https://jpinzon.shinyapps.io/Tornado_US/

Enjoy!!!
