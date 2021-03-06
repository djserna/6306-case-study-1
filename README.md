# Introduction
The Brewer's Association of American has supplied data to conduct a study into the hyper-competitive taste / alcohol content preferences of various states in the USA. This repository will contain all elements required to reproduce this data and findings to help our Brewer's make the best investment decisions.

## Files
The following files were supplied by BAA.

### Beers.csv

* Name: Name of the beer.
* Beer_ID: Unique identifier of the beer.
* ABV: Alcohol by volume of the beer.
* IBU: International Bitterness Units of the beer. 
* Brewery_ID: Brewery id associated with the beer. 
* Style: Style of the beer.
* Ounces: Ounces of beer.

### Breweries.csv

* Brew_ID: Unique identifier of the brewery.
* Name: Name of the brewery.
* City: City where the brewery is located.
* State: U.S. State where the brewery is located.

## Libraries
In an effort to streamline the code repository we have selected a few libraries to help with the heavy lifting:

### ggplot2
[ggplot2](http://ggplot2.tidyverse.org) is a system for declaratively creating graphics, based on The Grammar of Graphics. You provide the data, tell ggplot2 how to map variables to aesthetics, what graphical primitives to use, and it takes care of the details.

### DataExplorer
[DataExplorer](https://cran.r-project.org/web/packages/DataExplorer/index.html) is a data exploration process for data analysis and model building, so that users could focus on understanding data and extracting insights. The package automatically scans through each variable and does data profiling. Typical graphical techniques will be performed for both discrete and continuous features.

### dplyr
[dplyr](https://dplyr.tidyverse.org) is a fast, consistent tool for working with data frame like objects, both in memory and out of memory.

## Objects created
As a result of assimilating the data in a meaningful way, various objects have been created to help ensure the results and summaries are as readable as possible.

### Functions

* propmiss --> Find missing values

### Data Objects

* Beers --> Data that contains ABV and IBU values
* Breweries --> Name of brewery with city and state
* StateDB --> R internal data states information
* DistrictColumbia --> DC row to add to the StateDB object
* BreweryCounts --> Number of breweries by state
* BeersAndBreweries --> Merged data set of the beers and breweries
* BeerColumnInventory_nacount --> Table that shows missing values
* DF_ABV_IBU --> Data frame state, ABV and IBU
* DF_ABV_IBU_noNA --> Removes all missing values from the 'DF_ABV_IBU' table
* MEDIAN_ABV_IBU_by_State --> MEDIAN values for ABV&IBU by State
* BarPlot_ABV_byState --> Bar plot of median ABV by state
* BarPlot_IBU_byState --> Bar plot of median IBU by state
* MAX_ABV_byState --> MAX ABV with State
* MAX_IBU_byState --> MAX IBU with State
* SUMMARY_ABV --> Summary Stats of the ABV variable
* ABUvsIBU --> Scatter plot ABV vs IBU and color by StateRegion