# PacFIN.Utilities

## Introduction
Age-structured stock assessment models were developed to reconstruct historical abundance from age-, length-, and weight-composition data as well as other inputs. Data are often provided for individual fish, and these data must be summarized by year or season to be of use as input to a stock assessment model. These data provide information on growth, cohort strength, and fleet-specific selectivity.
`PacFIN.Utilities` offers a framework to summarize information collected from individual fish off of the U.S. West Coast and available within the Pacific Fisheries Information Network (PacFIN) that can easily be used as input for Stock Synthesis (SS). Specifically, the framework

  * filters the raw data stored in PacFIN,
  * expands samples taken from a tow to the trip level,
  * expands trips for a given state to state-specific catches, and
  * provides year-specific sample sizes and summaries of fish-specific data as yearly, fishery-specific compositions.

## Data
Data collection programs for groundfish managed within the Pacific Fisheries Management Council (PFMC) are the responsibility of individual states. Washington and Oregon have mandatory sampling protocols, while sampling in California is not mandatory. These programs have largely been in existence since the 1960s. PacFIN, which was initiated in 1981, serves as a central repository for these data. For fish landed whole, fork or total length and sex are typically recorded at a minimum. Otoliths are sometimes extracted and weights are sometimes taken. For fish landed dressed, dorsal length is taken rather than fork length (or total length). 
Samples can be taken from unsorted landings (i.e., 'ocean run') or from sorted categories (e.g., small, medium, large, ...). When fish are sorted into categories, samples are in theory taken from each sorted group and the actual weight of the sorted group is recorded. 

## Filtering
### Agencies
All samples from agencies other than Washington, Oregon, and California, i.e, tribal sampling, are removed. 

### Sampling
Random sampling method is the preferred sampling method. Market samples are the preferred sampling type. 

### Ages
Ages are first taken from the best age available and then from age readers one to three in that order. These subsequent ages are only used if the previous age is `NA`. 

### Lengths
Fork length is the primary length type used if available. Secondarily, standard length and then total length become the measurement of choice. 

#### Other length types
For other length types, i.e., dorsal, unknown, PacFIN.Utilities will first look to see if there is a valid fork length. Second, if there is not a fork-length measurement then it will use the general length information provided. 

#### Skates
Skates are unique in that lengths are not typically collected. Instead, inter-spiracle widths, collected by Washington, and disk widths are converted to lengths. 

### Oregon special-permit samples
Many samples were collected by Oregon using non-traditional sampling protocols in an effort to gain extra information about groundfish. For example, the Marine Recreational Information Program funded the collection of additional samples during the winter of 2011-2012. These 'special-permit' samples often do not collect all of the standard information collected by Oregon in their groundfish sampling program but they can provide additional auxiliary information for research topics related to but not directly informing stock assessments. 

On March 19, 2019, Oregon made a correction to PacFIN that properly designated a collection of special-permit samples as SPxxx samples rather than ORxxx samples. The SPxxx sample number provides a flag to stock assessment scientists that the sample should not be used. Prior to 2019, these samples from the late 1970s and early 1980s were included in the data utilized from PacFIN for age and length compositions. The incorrect sample numbers can be found in the object labeled `badORnums`.

### Sample weights
Oregon and California have gone to great lengths to provide correct species-specific weights of samples. This information is stored in `Pdata[, "EXP_WT"]` and `Pdata[, "SPECIES_WGT"]`, respectively. All Oregon and California samples that are missing this information are removed from the data set expanded to create compositions. 

## Expansion



### Stage-1 expansion



### Stage-2 expansion

