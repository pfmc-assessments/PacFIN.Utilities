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
Samples can be taken from unsorted landings (i.e., 'ocean run') or from sorted categories (e.g., small, medium, large, ...). When fish are sorted into categories, samples are in theory taken from each sorted group and the actual weight of the sorted group is recorded. Data within PacFIN contains a range of fields that provides information on each specific record: [area of landing](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/ar.txt), [gear type](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt), [port](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/pc.txt), [the condition landed (e.g., live, dead, dressed)](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/list_cl_condition.txt), and [the removal type (e.g., commercial, research)](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/list_cl_removal_type.txt). All records within PacFIN are associated with a unique species code [SPID](https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp.txt).

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
Expansions are performed to account for fish that were not sampled or when sampling is unequal across stratums. Expansions are calculated twice, first for fish with lengths and second for fish with ages because less fish are typically aged than lengthed and weights used for the expansions should only include fish that provide information. For example, if 10 fish were lengthed but only five fish were aged the expansion for the ages should only be based on the weight of the aged fish. 

### Stage-1 expansion
The stage-1 expansion is performed to account for fish that are not sampled in a given tow or trip. Sampled fish are thought to be representative of all fish within a tow. Thus, if only a few fish are sampled from a very large tow, these few samples will be expanded to represent a larger portion of the population than a sample consisting of the same or fewer number of fish that were sampled from a tow that wasn't as heavy. The expansion factor is the ratio of the weight of fish that were landed in the tow or trip to the weight of fish within that tow or trip that were sampled. 

#### Tow or trip weight
Unfortunately, the species-specific weight for a given tow or trip is not always recorded and states report the measurement in the following ways:

##### Washington
  * Round weight is the preferred measurement to account for the weight of fish that were dressed prior to weighing.
  * Total weight is used if round weight is not available.
  * Year- and gear-specific median tow or trip round weights are assigned to all samples that don't have a tow or trip weight. 
  * Year- and gear-specific median tow or trip total weights are assigned to all samples that don't have a tow or trip weight.

##### Oregon
  * `EXP_WT` is provided by Oregon as a measure of the weight the sample should be expanded to and defaults to the landing weight.
  * Years prior to 1973 are often missing `EXP_WT`, and for these instances, the weight is calculated in the same manner as the default method for California. 
  * Year- and gear-specific median tow or trip total weights are assigned to all samples that don't have a tow or trip weight.

##### California
  * The tow or trip weight, which includes the weight of all fish within a tow or trip that represent species that were sampled and can include species other than the species of concern, is multiplied by the ratio of the weight of the species of concern in the sample to the weight of the sample to get the fraction of the sample that contained the species of concern. It is assumed that the tow will be of this same ratio.
  * Year- and gear-specific median tow or trip total weights are assigned to all samples that don't have a tow or trip weight.

#### Sampled fish weight
Weights of the sample are found using the following three methods:

  1. Oregon provides the total weight of males, females, and unsexed fish within a given sample; these are summed to calculate the weight of all fish within the sample. 
  2. The species-specific weight for the species of concern within that sample is provided by California as `SPECIES_WGT`.
  3. The weight of all fish within the sample are summed. Fish weights are preferably empirically measured weights and secondarily calculated using a weight-length relationship. For fish in a sample without a length, the median length of all fish within the sample is used. 

### Stage-2 expansion
The stage-2 expansion typically operates at the state x gear x year level. Regardless, whatever level the stratum are defined at, the expansion is the ratio of the landings to the weight of fish that were sampled from those landings. 
