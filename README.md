# GCB-21-0250
Code and data for Callaghan et al. 2021. Thermal flexibility and a generalist life history promote urban affinity in butterflies. Global Change Biology.

The original data downloaded from GBIF can be downloaded here: **https://doi.org/10.15468/dl.p5nhwh**

This code was last ran on April 29th, 2021 under the following circumstances:

R version 4.0.3 (2020-10-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows Server x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] htmlwidgets_1.5.3   plotly_4.9.3        gbm_2.1.8           dismo_1.3-3         raster_3.4-5        sp_1.4-5            MASS_7.3-53         olsrr_0.5.3        
 [9] GGally_2.1.0        ggcorrplot_0.1.3    boot_1.3-25         fpc_2.2-9           TSclust_1.3.1       cluster_2.1.0       pdc_1.0.3           mgcv_1.8-33        
[17] nlme_3.1-149        patchwork_1.1.1     rnaturalearth_0.1.0 countrycode_1.2.0   forcats_0.5.0       stringr_1.4.0       purrr_0.3.4         readr_1.4.0        
[25] tibble_3.0.5        tidyverse_1.3.0     ggridges_0.5.3      ggplot2_3.3.3       concaveman_1.1.0    tidyr_1.1.2         sf_0.9-7            dplyr_1.0.3        

loaded via a namespace (and not attached):
  [1] readxl_1.3.1            backports_1.2.1         Hmisc_4.4-2             plyr_1.8.6              lazyeval_0.2.2          splines_4.0.3          
  [7] crosstalk_1.1.1         digest_0.6.27           htmltools_0.5.1.1       fansi_0.4.2             magrittr_2.0.1          checkmate_2.0.0        
 [13] openxlsx_4.2.3          modelr_0.1.8            xts_0.12.1              prettyunits_1.1.1       forecast_8.14           tseries_0.10-48        
 [19] jpeg_0.1-8.1            colorspace_2.0-0        rvest_0.3.6             haven_2.3.1             xfun_0.20               tcltk_4.0.3            
 [25] crayon_1.3.4            jsonlite_1.7.2          lme4_1.1-26             survival_3.2-7          zoo_1.8-8               glue_1.4.2             
 [31] gtable_0.3.0            webshot_0.5.2           car_3.0-10              kernlab_0.9-29          DEoptimR_1.0-8          prabclus_2.3-2         
 [37] quantmod_0.4.18         abind_1.4-5             scales_1.1.1            DBI_1.1.1               miniUI_0.1.1.1          Rcpp_1.0.6             
 [43] dtw_1.22-3              progress_1.2.2          viridisLite_0.3.0       xtable_1.8-4            htmlTable_2.1.0         units_0.6-7            
 [49] foreign_0.8-80          proxy_0.4-25            mclust_5.4.7            Formula_1.2-4           stats4_4.0.3            longitudinalData_2.4.1 
 [55] httr_1.4.2              RColorBrewer_1.1-2      modeltools_0.2-23       ellipsis_0.3.1          reshape_0.8.8           flexmix_2.3-17         
 [61] pkgconfig_2.0.3         farver_2.0.3            nnet_7.3-14             dbplyr_2.0.0            utf8_1.1.4              tidyselect_1.1.0       
 [67] labeling_0.4.2          rlang_0.4.10            manipulateWidget_0.10.1 reshape2_1.4.4          later_1.1.0.1           munsell_0.5.0          
 [73] cellranger_1.1.0        tools_4.0.3             cli_2.2.0               generics_0.1.0          broom_0.7.3             locpol_0.7-0           
 [79] fastmap_1.1.0           yaml_2.2.1              goftest_1.2-2           arm_1.11-2              knitr_1.30              fs_1.5.0               
 [85] zip_2.1.1               robustbase_0.93-7       rgl_0.105.22            clv_0.3-2.2             mime_0.9                xml2_1.3.2             
 [91] compiler_4.0.3          rstudioapi_0.13         curl_4.3                png_0.1-7               e1071_1.7-4             reprex_1.0.0           
 [97] statmod_1.4.35          stringi_1.5.3           rgeos_0.5-5             lattice_0.20-41         Matrix_1.2-18           classInt_0.4-3         
[103] nloptr_1.2.2.2          urca_1.3-0              vctrs_0.3.6             pillar_1.4.7            lifecycle_0.2.0         lmtest_0.9-38          
[109] data.table_1.13.6       httpuv_1.5.5            R6_2.5.0                latticeExtra_0.6-29     promises_1.1.1          rio_0.5.16             
[115] KernSmooth_2.23-17      gridExtra_2.3           codetools_0.2-16        assertthat_0.2.1        nortest_1.0-4           withr_2.4.1            
[121] fracdiff_1.5-1          diptest_0.75-7          parallel_4.0.3          hms_1.0.0               quadprog_1.5-8          grid_4.0.3             
[127] rpart_4.1-15            timeDate_3043.102       rnaturalearthdata_0.1.0 coda_0.19-4             class_7.3-17            minqa_1.2.4            
[133] misc3d_0.9-0            carData_3.0-4           TTR_0.24.2              shiny_1.6.0             lubridate_1.7.9.2       base64enc_0.1-3        
[139] tinytex_0.29     
