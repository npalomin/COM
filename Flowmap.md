Flowmap
================
NP
25/10/2018

Vyner Street geography of imports
---------------------------------

This is a log to register the process of creating a geographic-connections-map of imports. The purpose of this visualisation is to illustrate the inputs of the 'geography of production' of the firms interviewed for the 'Vyner Street' case study in Hackney in the context of the \[Cities of Making\]<http://citiesofmaking.com/> project.

------------------------------------------------------------------------

URLs visited for reference:
- <https://github.com/rafapereirabr/flow-map-in-r-ggplot/blob/master/Flow%20Map%20in%20R.R>
- <https://gis.stackexchange.com/questions/71921/list-of-central-coordinates-for-all-countries/71958>
- <https://www.r-graph-gallery.com/192-ggplot-themes/>
- <https://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/>

------------------------------------------------------------------------

###### Packages and Libraries

``` r
# Packages
#install.packages("rworldmap")
#install.packages("mapproj")
#install.packages("rworldxtra")

# Libraries
library(maps)
library(geosphere)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(plyr)
library(data.table)
library(ggthemes)
library(rgeos)
library(readr)
library(data.table)
library(mapproj)
library(tmap)
library(rworldxtra)
```

#### Part I: Prepare de dataset

###### 1. Load .csv file with 'From' and 'To' information

``` r
Mwf <- read_csv("/Volumes/ucfnnap-1/COM/material_world_flow.csv")
```

###### 2. Get worldmap (from rworldmap library) and get centroids

``` r
# get world map
wmap <- getMap(resolution="high")
# get centroids
centroids <- gCentroid(wmap, byid=TRUE)
```

###### 3. Get a dataframe with centroids

``` r
# get a data.frame with centroids
cent <- as.data.frame(centroids)
head(cent)
```

    ##                     x         y
    ## Aruba       -69.97345  12.51678
    ## Afghanistan  66.00845  33.83627
    ## Angola       17.53646 -12.29118
    ## Anguilla    -63.06082  18.22560
    ## Albania      20.05399  41.14258
    ## Aland        20.03715  60.20733

###### 4. Convert rownames 'rn' into a variable

``` r
# convert rownames into variable 
setDT(cent, keep.rownames = TRUE)[]
```

    ##                rn          x         y
    ##   1:        Aruba  -69.97345  12.51678
    ##   2:  Afghanistan   66.00845  33.83627
    ##   3:       Angola   17.53646 -12.29118
    ##   4:     Anguilla  -63.06082  18.22560
    ##   5:      Albania   20.05399  41.14258
    ##  ---                                  
    ## 249:        Samoa -172.16028 -13.75468
    ## 250:        Yemen   47.59093  15.90515
    ## 251: South Africa   25.08773 -29.00105
    ## 252:       Zambia   27.77421 -13.46011
    ## 253:     Zimbabwe   29.85188 -19.00254

###### 5. Merge 'Wmf' with 'cent' dataframe by the columns that contain country names 'From' and 'rn'. Verify that the names of the countries are equally spelled. Check the number of observations in your 'joined' object.

``` r
# merge material_world_flow with centroids
odm <- merge(Mwf, cent, by.x="From", by.y="rn", all.x=T)
head(odm)
```

    ##        From           Import      To  x.x  y.x     yend     xend     Type
    ## 1 Argentina     Sheep’s wool Hackney <NA> <NA> 51.54032 -0.06039 Material
    ## 2    Brazil             Jars Hackney <NA> <NA> 51.54032 -0.06039  Product
    ## 3     China     Metal frames Hackney <NA> <NA> 51.54032 -0.06039  Product
    ## 4   China 1           Labels Hackney <NA> <NA> 51.54032 -0.06039  Product
    ## 5   China 2 Electronic chips Hackney <NA> <NA> 51.54032 -0.06039  Product
    ## 6   Germany          Threads Hackney <NA> <NA> 51.54032 -0.06039  Product
    ##          Region       x.y       y.y
    ## 1 International -65.17822 -35.38270
    ## 2 International -53.09397 -10.78278
    ## 3 International 103.82491  36.56128
    ## 4 International        NA        NA
    ## 5 International        NA        NA
    ## 6   Continental  10.38155  51.10621

###### 6.Delete empty columns by index (4,5)

``` r
# specify columns to remove with negative index
odm <- odm[, -c(4, 5)]
```

###### 7. Given that some countries were repeated (same centroid) and we would want our connections not to overlap, we named those countries as 'subsidiaries'. Therefore, after the merge in point 5. the 'x' and 'y' fields remained empty (NA). We now need to get the coordinates for this location (or destination) in columns 'x.y' (long) and 'y.y' (lat). The code checks the name of the country and asigns a value to the respective cell (based on the 'parent' country). Now our dataset is complete.

``` r
odm$x.y[odm$From == "China 1"] <- odm$x.y[odm$From == "China"]-3
odm$y.y[odm$From == "China 1"] <- odm$y.y[odm$From == "China"]-3
odm$x.y[odm$From == "China 2"] <- odm$x.y[odm$From == "China"]-6
odm$y.y[odm$From == "China 2"] <- odm$y.y[odm$From == "China"]-6

odm$x.y[odm$From == "Germany 1"] <- odm$x.y[odm$From == "Germany"]-0.5
odm$y.y[odm$From == "Germany 1"] <- odm$y.y[odm$From == "Germany"]-0.5
odm$x.y[odm$From == "Germany 2"] <- odm$x.y[odm$From == "Germany"]-1
odm$y.y[odm$From == "Germany 2"] <- odm$y.y[odm$From == "Germany"]-1

odm$x.y[odm$From == "Italy 1"] <- odm$x.y[odm$From == "Italy"]+0.5
odm$y.y[odm$From == "Italy 1"] <- odm$y.y[odm$From == "Italy"]+0.5
odm$x.y[odm$From == "Italy 2"] <- odm$x.y[odm$From == "Italy"]+1
odm$y.y[odm$From == "Italy 2"] <- odm$y.y[odm$From == "Italy"]+1
```

#### Part II. Create a simple map using Curve Line

###### 8. Get worldmap from library 'rworldxtra' to use as a 'basemap' (with high resolution). Then convert to dataframe with the 'fortify' function.

``` r
worldMap <- getMap(resolution = "high")
mapworld_df <- fortify(worldMap)
```

    ## Regions defined for each Polygons

``` r
class(mapworld_df)
```

    ## [1] "data.frame"

###### 9. Create a test plot for Europe. First, get the boundingbox to determine the 'xlim' and 'ylim' of the plot. This URL will help <https://boundingbox.klokantech.com/> .Copy the geojson to get the coordinates to create the 'xlim1' and 'ylim1' objects.

``` r
# [[[-10.8421722152,34.0420631631],[45.1606791686,34.0420631631],[45.1606791686,59.3006822903],[-10.8421722152,59.3006822903],[-10.8421722152,34.0420631631]]]
xlim1 <- c(-23.3537208175, 51.9816177994)
ylim1 <- c(30.0673444226, 61.7436916491)
eu <- map("world", col="white", fill=TRUE, bg="#f2f2f2", lwd=0.05, xlim=xlim1, ylim=ylim1)
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-10-1.png)

###### 10. Before we continue we'll get another worldmap with less resolution to use when ploting tests and save time.

``` r
L_worldMap <- getMap(resolution = "low")
L_mapworld_df <- fortify(L_worldMap)
```

###### 11. To creat a map we use ggplot() to plot the 'basemap' layer and the 'connections' in the 'odm' dataset. The 'basemap' layer will be a geom\_polygon object and the 'connections' a geom\_curve. Line colour and type are set according to 'Import' and 'Type' respectively.

``` r
conn <- geom_curve(data = odm, aes(x = x.y, y = y.y, xend = xend, yend = yend, color=Import, linetype=Type), curvature = 0.1)
bmap <- geom_polygon(data= L_mapworld_df, aes(long,lat, group=group), fill="white")

ggplot() + bmap + conn
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-12-1.png)

###### 12. Let's try interchanging color and linetype.

``` r
conn_v <- geom_curve(data = odm, aes(x = x.y, y = y.y, xend = xend, yend = yend, color=Type, linetype=Import), curvature = 0.1)

ggplot() + bmap + conn_v
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-13-1.png)

###### 13. The maps show a general view of the materials and products import, being the first one a bit more clearer. But to be able have a more precise differentiation of the materials we'll try to create maps according to 'Region' using the facet\_wrap() function.

``` r
ggplot() + bmap + conn + facet_wrap(~Region, dir = "v")
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-14-1.png)

###### 14. Another alternative is to create two separate maps with basemaps according to the geographical extent of the 'Region'. To do that we subset the dataset accordingly and create two new dataframes.

``` r
odm_cont <- odm[odm$Region == "Continental",]
odm_int <- odm[odm$Region == "International",]
```

###### 15. Let's try creating the 'Continental' map. To 'zoom in' to the 'Region' of interest we create the 'xlim2' and 'ylim2' that we'll set using the coord\_cartesian() function.

``` r
xlim2 <- c(-10.7306956086, 39.9190882164)
ylim2 <- c(33.9565350786, 58.1373679576)

conn_cont <- geom_curve(data = odm_cont, aes(x = x.y, y = y.y, xend = xend, yend = yend, color=Import, linetype=Type), curvature = 0.1)
bmap <- geom_polygon(data= L_mapworld_df, aes(long,lat, group=group), fill="white")

ggplot() + bmap + conn_cont + coord_cartesian(xlim=xlim2, ylim=ylim2)
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-16-1.png)

###### 16. Now, it is possible to see, for example, that cotton comes from Greece. We can now improve the aesthetics of the map. We'll start by creating our own color palettes.

``` r
com6=c("#ffeb28","#85db49","#2bfd94","#66aa93","#358189","#2c576f")
com8=c("#ff6347","#e2fa5b","#96ee42","#60c854","#2bfd94","#668e81","#358daf","#5c47cf")

# Function for plotting colors side-by-side
pal <- function(col, border = "light gray", ...){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

pal(com6)
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
pal(com8)
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-17-2.png)

###### 17. We can use the theme\_map() to quickly assign a 'maplike' style to our plot. We'll also customize our geom\_polygon to create a better basemap and we'll use the scale\_colour\_manual() function to apply our palettes.

``` r
bmap <- geom_polygon(data= mapworld_df, aes(long,lat, group=group), fill="grey95", colour="grey65", size=0.03)

ggplot() + bmap + conn_cont + coord_cartesian(xlim=xlim2, ylim=ylim2) + 
  theme_map() + 
  scale_color_manual(values = com8) + 
  theme(legend.position="top", legend.title = element_text(face = "italic", family = "serif"))
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-18-1.png)

###### 18. Next we can create the 'International' map with similar aesthetics.

``` r
conn_int <- geom_curve(data = odm_int, aes(x = x.y, y = y.y, xend = xend, yend = yend, color=Import, linetype=Type), curvature = 0.1)

ylim3 <- c(-60, NA)

L_bmap <- geom_polygon(data= L_mapworld_df, aes(long,lat, group=group), fill="grey95", colour="grey65", size=0.03)

ggplot() + L_bmap + conn_int + coord_cartesian() + ylim(ylim3) +
  theme_map() + 
  scale_color_manual(values = com8) + 
  theme(legend.position="top", legend.title = element_text(face = "italic", family = "serif"))
```

![](Flowmap_files/figure-markdown_github/unnamed-chunk-19-1.png)
