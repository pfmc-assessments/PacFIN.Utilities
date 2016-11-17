#' Example PacFIN Run
#' 
#' This is an example run of the PacFIN Utilities workflow.  
#' 
#' \code {
#' > library(PacFIN.Utilities)
#' 
#' > sink(file = "~/Example.txt", split=T)
#' > dim(PacFIN.BDS.XMPL)
#' 
#' [1] 10000    56
#' 
#' > dim(PacFIN.Catch.XMPL)
#' 
#' [1] 16 10
#' 
#' > Pdata = cleanPacFIN(PacFIN.BDS.XMPL)
#' 
#' Cleaning data
#' 
#' These values have been initialized for use when comps are generated.
#' Use Stratify and getSeason to reset them to appropriate values.
#' 
#' Pdata$ fleet  = 1
#' Pdata$ fishery  = 1
#' Pdata$ season  = 1
#' 
#' Getting state information from SOURCE_AGID 
#' 
#' 
#' Original data: 
#' 
#'    C    O    W 
#' 1138 3374 5488 
#' 
#' 
#' Data retained: 
#' 
#'   CA   OR   WA 
#' 1138 3374 5488 
#' 
#' 
#'  0  records were removed because no state id could be assigned
#' 
#' Pdata$state is initialized to Pdata$SOURCE_AGID
#' Pdata$fishyr is initialized to Pdata$SAMPLE_YEAR
#' 
#' 
#' Gear groupings reflect those in the table at http://pacfin.psmfs.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt
#' 
#' Pdata$fishyr is initialized to Pdata$SAMPLE_YEAR
#' 
#' Removal Report
#' 
#' Records in input:                  10000 
#' Records not in INPFC_AREA:         0 
#' Records in badRecords list:        0 
#' Records with bad SAMPLE_TYPE       168 
#' Records with bad SAMPLE_METHOD     11 
#' Records with no SAMPLE_NO          0 
#' Records with no usable length      6 
#' Records remaining:                 9815 
#' 
#' 
#' > table(Pdata$geargroup)
#' 
#'  HKL  NET  TLS  TWL  TWS 
#'  726  106   30 8520  425 
#' 
#' > Pdata$mygear = Pdata$geargroup
#' > Pdata$mygear[ ! Pdata$mygear %in% c("TWL","HKL")] = "OTHER"
#' > Pdata$stratification = paste(Pdata$state,Pdata$mygear, sep=".")
#' > table(Pdata$stratification)
#' 
#'   CA.HKL CA.OTHER   CA.TWL   OR.HKL OR.OTHER   OR.TWL   WA.HKL WA.OTHER   WA.TWL 
#'      618      130      389       98      160     3113       10      279     5018 
#' 
#' > head(PacFIN.Catch.XMPL)
#'   Year CA.HKL CA.OTHER CA.TWL OR.HKL OR.OTHER  OR.TWL WA.TWL WA.HKL WA.OTHER
#' 1 2000   8.67       NA  48.13   0.09    47.25 1662.34 658.56   0.95     1.07
#' 2 2001   4.24       NA  61.55   0.04    30.03  975.03 756.14   1.04     0.77
#' 3 2002   0.50       NA  17.07   0.51     2.67  296.22 601.75   0.76       NA
#' 4 2003   0.25       NA   1.41   0.03       NA   39.06 332.00   0.54       NA
#' 5 2004   0.03       NA   3.24   0.22       NA   73.50 425.59   3.68       NA
#' 6 2005   0.34       NA   0.63   0.09       NA   32.46 618.61   0.67       NA
#' 
#'  Pdata = getExpansion_1(Pdata)
#' 
#' Individual weights will be generated from the following values:
#' 
#' Females: 2e-06 3.5 Males: 2e-06 3.5 Unknowns: 2e-06 3.5 
#' 
#' 
#' Done calculating sample weights
#' 
#'       M+F          SPECIES_WT          L-W          Final Wt_Sampled
#'  Min.   :  0.0   Min.   :  1.00   Min.   : 0.1966   Min.   :  0.00  
#'  1st Qu.: 98.8   1st Qu.: 21.00   1st Qu.: 5.2028   1st Qu.: 12.33  
#'  Median :101.6   Median : 45.00   Median : 9.5179   Median : 29.93  
#'  Mean   :149.1   Mean   : 46.93   Mean   :13.1159   Mean   : 65.05  
#'  3rd Qu.:175.2   3rd Qu.: 65.00   3rd Qu.:16.5574   3rd Qu.: 99.40  
#'  Max.   :515.5   Max.   :150.00   Max.   :63.8474   Max.   :515.50  
#'  NA's   :6535    NA's   :8678                                       
#' 
#' Wt_Methods:
#' 
#'    1    2    3 
#' 3280 1137 5398 
#' 
#' Sample Wts found for all samples.
#' 
#' 
#' Sampled pounds per trip:
#' 
#'     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#'      0.0    873.5   4914.0  15880.0  18000.0 307600.0      389 
#' 
#' 
#' WA expansions set to 1. Fish tickets do not represent whole trips in WA.
#' 
#' 
#'  0 NA Expansion_Factor_1 values replaced by 1.
#' 
#' 
#' Maximum expansion capped at 0.95 quantile: 124.4205 
#' 
#' 
#' Capping Expansion_Factor_1 at  0.95 
#' 
#'    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#'    1.00    1.00    1.00   17.92   16.97  124.40 
#' 
#' > Pdata = getExpansion_2(Pdata, PacFIN.Catch.XMPL, Convert=T)
#' 
#' Converting Catch to pounds (multiplying by 2204). 
#' 
#' ................Assigned catch for  CA.HKL 
#' ................Assigned catch for  CA.OTHER 
#' ................Assigned catch for  CA.TWL 
#' ................Assigned catch for  OR.HKL 
#' ................Assigned catch for  OR.OTHER 
#' ................Assigned catch for  OR.TWL 
#' ................Assigned catch for  WA.TWL 
#' ................Assigned catch for  WA.HKL 
#' ................Assigned catch for  WA.OTHER 
#' 
#' Summary of Expansion_Factor_2
#' 
#'    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#'   1.000   1.000   1.000   2.669   1.379 651.000 
#' 
#'  0 NA Expansion_Factor_2 values replaced by 1.
#' 
#' 
#' Maximum expansion capped at 0.95 quantile: 8.94264 
#' 
#' Summary of Expansion_Factor_2
#' 
#'    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#'   1.000   1.000   1.000   2.133   1.379   8.943 
#' 
#' Remember to set (or reset) Pdata$Final_Sample_Size
#' 
#' > Lcomps = getComps(Pdata, Comps="LEN")
#' 
#' 
#' Aggregating, stratification is by fleet, fishyr, season, lengthcm 
#' 
#' > Lcomps = doSexRatio(Lcomps)
#' 
#' Applying sex ratio: 0.5,0.5,1 to numbers, samples and tows
#' 
#' 
#' Done.
#' 
#' > writeComps(Lcomps, fname = "~/LenComps.csv")
#' 
#' Writing comps to file ~/LenComps.csv 
#' 
#' Note that if you didn't run doSexRatio, all unsexed fish disappear at this point.
#' 
#' No length bins provided, using data as-is
#' 
#' 48 unique keys for 1102 records
#' 
#' Assembling, sex is: m 
#' Assembling, sex is: f 
#' Assembling, sex is: b 
#' Writing F only, dimensions: 47 82 
#' Writing M only, dimensions: 47 82 
#' Writing combined sexes as females, dimensions: 48 82  
#' 
#' Writing FthenM, dimensions: 48 82 
#' 
#' > Adata = cleanAges(Pdata)
#' 
#' Cleaning Age data.
#' 
#' Removal report
#' 
#' Records in input:                   9815 
#' Records with age less than min:     3154 
#' Records with bad agemethods:        208 
#' Records remaining:                  6453 
#' 
#' > Adata = getExpantion_1(Adata)
#' 
#' Individual weights will be generated from the following values:
#' 
#' Females: 2e-06 3.5 Males: 2e-06 3.5 Unknowns: 2e-06 3.5 
#' 
#' 
#' Done calculating sample weights
#' 
#'       M+F          SPECIES_WT        L-W         Final Wt_Sampled
#'  Min.   :  0.0   Min.   :  1    Min.   : 0.451   Min.   :  0.00  
#'  1st Qu.: 99.1   1st Qu.: 21    1st Qu.: 5.187   1st Qu.:  9.47  
#'  Median :101.8   Median : 38    Median : 8.519   Median : 18.60  
#'  Mean   :150.3   Mean   : 43    Mean   : 9.629   Mean   : 69.07  
#'  3rd Qu.:177.0   3rd Qu.: 52    3rd Qu.:12.870   3rd Qu.:100.50  
#'  Max.   :515.5   Max.   :101    Max.   :34.461   Max.   :515.50  
#'  NA's   :3845    NA's   :6101                                    
#' 
#' Wt_Methods:
#' 
#'    1    2    3 
#' 2608  352 3493 
#' 
#' Sample Wts found for all samples.
#' 
#' 
#' Sampled pounds per trip:
#' 
#'    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#'       0    1155    4939   12870   13730  307600     260 
#' 
#' 
#' WA expansions set to 1. Fish tickets do not represent whole trips in WA.
#' 
#' 
#'  0 NA Expansion_Factor_1 values replaced by 1.
#' 
#' 
#' Maximum expansion capped at 0.95 quantile: 140.4484 
#' 
#' 
#' Capping Expansion_Factor_1 at  0.95 
#' 
#' > Adata = getExpansion_2(Adata, PacFIN.Catch.XMPL, Convert=T)
#' 
#' Converting Catch to pounds (multiplying by 2204). 
#' 
#' ................Assigned catch for  CA.HKL 
#' ................Assigned catch for  CA.OTHER 
#' ................Assigned catch for  CA.TWL 
#' ................Assigned catch for  OR.HKL 
#' ................Assigned catch for  OR.OTHER 
#' ................Assigned catch for  OR.TWL 
#' ................Assigned catch for  WA.TWL 
#' ................Assigned catch for  WA.HKL 
#' ................Assigned catch for  WA.OTHER 
#' 
#' Summary of Expansion_Factor_2
#' 
#'    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#'   1.000   1.000   1.000   3.265   1.000 233.600 
#' 
#'  0 NA Expansion_Factor_2 values replaced by 1.
#' 
#' 
#' Maximum expansion capped at 0.95 quantile: 11.71813 
#' 
#' Summary of Expansion_Factor_2
#' 
#'    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#'   1.000   1.000   1.000   2.452   1.000  11.720 
#' 
#' Remember to set (or reset) Pdata$Final_Sample_Size
#' 
#' 
#' > Adata$Final_Sample_Size = Adata$Expansion_Factor_1 * Adata$Expansion_Factor_2
#' 
#' > Acomps = getComps(Adata, Comps="AGE")
#' 
#' Aggregating, stratification is by fleet, fishyr, season, lengthcm, age 
#' 
#' > Acomps = doSexRatio(Acomps)
#' 
#' Applying sex ratio: 0.5,0.5,1 to numbers, samples and tows
#' 
#' 
#' Done.
#' 
#' > writeComps(Acomps, fname= "~/Age.comps.csv")
#' 
#' Writing comps to file ~/Age.Comps.csv 
#' 
#' Note that if you didn't run doSexRatio, all unsexed fish disappear at this point.
#' 
#' 
#' No age bins provided, using data as-is
#' 
#' 43 unique keys for 896 records
#' 
#' 
#' Assembling, sex is: m 
#' Assembling, sex is: f 
#' Assembling, sex is: b 
#' Writing F only, dimensions: 43 100 
#' Writing M only, dimensions: 43 100 
#' Writing combined sexes as females, dimensions: 43 100  
#' 
#' Writing FthenM, dimensions: 43 100 
#' 
#' > Adata$Final_Sample_Size = 1
#' 
#' > ALcomps = getComps(Adata, Comps="AAL")
#' 
#' Aggregating, stratification is by fleet, fishyr, season, lengthcm, age 
#' 
#' > ALcomps = doSexRatio(ALcomps)
#' 
#' Applying sex ratio: 0.5,0.5,1 to numbers, samples and tows
#' 
#' 
#' Done.
#' 
#' > writeComps(ALcomps, fname = "~/AAL.Comps.csv")
#' 
#' Writing comps to file ~/AAL.Comps.csv 
#' 
#' Note that if you didn't run doSexRatio, all unsexed fish disappear at this point.
#' 
#' 
#' No length bins provided, using data as-is
#' 
#' No age bins provided, using data as-is
#' 
#' 882 unique keys for 3644 records
#' 
#' Assembling, sex is: m 
#' Assembling, sex is: f 
#' Assembling, sex is: b 
#' Writing F only, dimensions: 756 101 
#' Writing M only, dimensions: 640 101 
#' Writing combined sexes as females, dimensions: 882 101  
#' 
#' Writing FthenM, dimensions: 882 101 
#' 
#' > sink()
#' }
#'
NULL
