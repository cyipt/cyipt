

```r
# road scenarios
knitr::knit(input = "model-uptake.Rmd")
```

```
## Warning in readLines(if (is.character(input2)) {: cannot open file 'model-
## uptake.Rmd': No such file or directory
```

```
## Error in readLines(if (is.character(input2)) {: cannot open the connection
```

```r
typology = readr::read_csv("input-data/roadtypes4.csv")
```

```
## Error: 'input-data/roadtypes4.csv' does not exist in current working directory ('/home/robin/cyipt/cyipt/scripts/select_infra').
```

```r
rc = readRDS("../example-data/bristol/results/osm-schemes.Rds")
```

```
## Warning in gzfile(file, "rb"): cannot open compressed file '../example-
## data/bristol/results/osm-schemes.Rds', probable reason 'No such file or
## directory'
```

```
## Error in gzfile(file, "rb"): cannot open the connection
```

```r
summary(ways$length)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##    0.212   28.490   65.004  134.582  147.697 7632.609
```

```r
select(ways, contains("cycle")) %>% summary()
```

```
##        bicycle      bicycle.oneway cyclestreets_id          cycleway    
##  yes       :  903   no  :    1     12686:    1     track        :  218  
##  designated:  450   NA's:28834     47279:    1     lane         :   77  
##  no        :  321                  73802:    1     opposite     :   42  
##  dismount  :   24                  NA's :28832     no           :   22  
##  permissive:   20                                  opposite_lane:   21  
##  (Other)   :    7                                  (Other)      :   19  
##  NA's      :27110                                  NA's         :28436  
##       cycleway.left   cycleway.left.width cycleway.oneside.width
##  designated  :    5   1.75:    4          1.75:    1            
##  lane        :   70   NA's:28831          NA's:28834            
##  share_busway:   24                                             
##  NA's        :28736                                             
##                                                                 
##                                                                 
##                                                                 
##  cycleway.otherside cycleway.otherside.width        cycleway.right 
##  lane:    1         1.75:    3               lane          :   32  
##  NA's:28834         NA's:28832               opposite_lane :    4  
##                                              opposite_track:    3  
##                                              share_busway  :    5  
##                                              yes           :    2  
##                                              NA's          :28789  
##                                                                    
##  cycleway.right.width      motorcycle   
##  1.75:    2           designated:    3  
##  NA's:28833           no        :    3  
##                       permissive:    1  
##                       private   :    2  
##                       yes       :    4  
##                       NA's      :28822  
##                                         
##               motorcycle.conditional oneway.bicycle ramp.bicycle
##  no @ (Mo-Su 07:00-19:00):    3      no  :   24     yes :    5  
##  NA's                    :28832      NA's:28811     NA's:28830  
##                                                                 
##                                                                 
##                                                                 
##                                                                 
##                                                                 
##                                       source.bicycle 
##  painted sign on path at Ladies Mile         :    1  
##  painted sign on path at Upper Belgrave Place:    1  
##  NA's                                        :28833  
##                                                      
##                                                      
##                                                      
##                                                      
##           geometry    
##  LINESTRING   :28835  
##  epsg:4326    :    0  
##  +proj=long...:    0  
##                       
##                       
##                       
## 
```

```r
ways_long = dplyr::filter(ways, length > 500)
ways_long$length_cycleway = ways_long$length
summary(ways_long$cycleway)
```

```
##                  lane                    no              opposite 
##                     4                     5                     0 
##         opposite_lane opposite_share_busway        opposite_track 
##                     0                     0                     0 
##          share_busway                shared                 track 
##                     1                     0                    23 
##                   yes                  NA's 
##                     0                  1303
```

```r
l_joined = stjoin_old(lfq$rf, ways_long["length_cycleway"], FUN = sum) # old way
```

```
## although coordinates are longitude/latitude, it is assumed that they are planar
```

```r
summary(l_full$length_cycleway)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   504.5  6038.5  9882.8 11943.0 15691.5 49675.7      17
```

```r
summary(l_joined$length_cycleway) # 5 fold increase in cycling
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   504.5  6038.5  9882.8 11943.0 15691.5 49675.7      17
```

```r
l_full$length_cycleway = l_joined$length_cycleway
uptake = predict(m8, as.matrix(l_full[-c(1, 2)]))
cor(uptake * l$all, l_full$bicycle)^2
```

```
## [1] 0.8596601
```

```r
mean(uptake)
```

```
## [1] 0.06994756
```

```r
mean(l_full$bicycle / l_full$all)
```

```
## [1] 0.05262558
```

