# EDA for CCDSS 2012 summary
A.Koval & K.Moselle  
`r Sys.Date()`  

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->




<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 



<!-- Load the sources.  Suppress the output when loading sources. --> 

# I. Exposition

## Glossary
Review object definitions to assist you in reading the report. 
<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 


Review functions definitions to assist you in reading the report.


<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


## Load Data
<!-- Load the datasets.   -->

```r
# see ./data-unshared/contents.md for origin of the data
ds <- readr::read_csv("./data-unshared/raw/ccdss-scsmc-eng.csv")
```

```
Warning: 42588 parsing failures.
 row                                                               col   expected actual
3345 Hospitalizations with the disease case definition person count    an integer    N/A
3345 Hospitalizations with the disease case definition (Separations)   an integer    N/A
3345 Hospitalizations with the disease case definition (Days Stayed)   an integer    N/A
3345 Hospitalizations without the disease case definition person count an integer    N/A
3345 Hospitalizations without the disease (Separations)                an integer    N/A
.... ................................................................. .......... ......
See problems(...) for more details.
```

<!-- Inspect the datasets.   -->


## Tweak Data
<!-- Tweak the datasets.   -->


# II. Development A



## A1


## A2


## A3


## A4


## A5





# III. Development B


## B1


## B2


## B3


## B4


## B5



# IV. Recap


## R1


## R2


## R3








```
Report rendered by koval_000 at 2017-05-13, 10:16 -0400
```

```
R version 3.3.2 (2016-10-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] knitr_1.15.1                TabularManifest_0.1-16.9001 RColorBrewer_1.1-2          dichromat_2.0-0            
[5] ggplot2_2.2.1               extrafont_0.17              dplyr_0.5.0                 magrittr_1.5               

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.9      Rttf2pt1_1.3.4   munsell_0.4.3    testit_0.6       colorspace_1.3-2 R6_2.2.0        
 [7] stringr_1.1.0    plyr_1.8.4       tools_3.3.2      gtable_0.2.0     DBI_0.5-1        extrafontdb_1.0 
[13] htmltools_0.3.5  yaml_2.1.14      lazyeval_0.2.0   assertthat_0.1   rprojroot_1.2    digest_0.6.12   
[19] tibble_1.2       readr_1.0.0      tidyr_0.6.1      evaluate_0.10    rmarkdown_1.3    stringi_1.1.2   
[25] scales_0.4.1     backports_1.0.5 
```



