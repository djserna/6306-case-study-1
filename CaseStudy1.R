#install.packages("DataExplorer")
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("usmap")

library(ggplot2)
library(DataExplorer)
library(dplyr)
library(usmap)
#library(knitr)

##############################################
#####                          ###############
#####         Load Data        ###############
#####        Basic details     ###############
#####                          ###############
##############################################

#Read CSV file into R
Beers <- read.csv("Beers.csv", header=TRUE, sep=",", strip.white = TRUE)
Breweries <- read.csv("Breweries.csv", header=TRUE, sep=",", strip.white = TRUE)

#find dimensions of data frames
dim(Beers)
dim(Breweries)

#list column names of data frames
colnames(Beers)
colnames(Breweries)

#Get classification
class(Beers)
class(Breweries)

##############################################
#####                          ###############
#####                          ###############
#####    Make State DB         ###############
#####                          ###############
##############################################

#Create State DB data frame
StateDB <- data.frame(state.name, state.abb, state.region)
colnames(StateDB)[colnames(StateDB)=='state.name'] <- 'StateName'
colnames(StateDB)[colnames(StateDB)=='state.abb'] <- 'State'
colnames(StateDB)[colnames(StateDB)=='state.region'] <- 'StateRegion'

#Add district of Columbia to StateDB Data Frame
DistrictColumbia <- data.frame("District of Columbia","DC", "South")
names(DistrictColumbia) <- c("StateName","State", "StateRegion")
StateDB <- rbind(StateDB, DistrictColumbia)

head(StateDB,2)

##############################################
#####                          ###############
#####         Question 1       ###############
#####    Breweries per state   ###############
#####                          ###############
##############################################
#count of breweriews by state
BreweryCounts <- data.frame(table(Breweries$State))

#rename column names
colnames(BreweryCounts)[colnames(BreweryCounts)=='Var1'] <- 'State'
colnames(BreweryCounts)[colnames(BreweryCounts)=='Freq'] <- 'NumberOfBreweriesByState'

#Merge the StateDB and sort by count of breweries by state
BreweryCounts <- merge(BreweryCounts, StateDB, by.x=("State"), by.y=("State"))
BreweryCounts <- BreweryCounts[order(BreweryCounts$NumberOfBreweriesByState, decreasing=TRUE),c(3,2)]
head(BreweryCounts)

#Merge BreweryCounts to the statepop data set by state name
BreweryCountsMap <- merge(statepop, BreweryCounts, by.x=("full"), by.y=("StateName"))

#Plot states and color by brewery counts
usmap::plot_usmap(data = BreweryCountsMap,
                        values = "NumberOfBreweriesByState",
                        lines = "black") + scale_fill_continuous(
                        low = "green",
                        high = "red",
                        name = "Brewery Count",
                        label = scales::comma) + theme(legend.position = "right")+
                        labs(title = "Number of Breweries per State")

##############################################
#####                          ###############
#####         Question 2       ###############
#####       Merge Data Sets    ###############
#####                          ###############
##############################################

#join data on Brewery_id and Brew_ID 
BeersAndBreweries <- merge(Beers, Breweries, by.x=("Brewery_id"), by.y=("Brew_ID"))

#list column names on the joined data frame
colnames(BeersAndBreweries)

#rename the name.x(Beer) and name.y(Brewery) after the merger
colnames(BeersAndBreweries)[colnames(BeersAndBreweries)=='Name.x'] <- 'BeerName'
colnames(BeersAndBreweries)[colnames(BeersAndBreweries)=='Name.y'] <- 'BreweryName'

#Merge data with State DB
BeersAndBreweries <- merge(BeersAndBreweries, StateDB, by="State", all = TRUE)

#find dimensions of data frames
dim(BeersAndBreweries)
dim(BeersAndBreweries)

#Show first and last 6 entries of merged files
head(BeersAndBreweries)
tail(BeersAndBreweries)

##############################################
#####                          ###############
#####         Question 3       ###############
#####    Report NAs in columns ###############
#####                          ###############
##############################################

#Graphical representation of missing vaules using 'DataExporer' library
plot_missing(BeersAndBreweries, title = "Percent of Missing Values")

#Function to count all NA's in columns (stole from the internet)
#http://www.gettinggeneticsdone.com/2011/02/summarize-missing-data-for-all.html
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      na_count=sum(is.na(x)),
      Obs=length(x), 
      perc_missing=sum(is.na(x))/length(x)*100
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$na_count, decreasing=TRUE), ])
}

#show results of NA's counted
BeerColumnInventory_nacount <- propmiss(BeersAndBreweries)
BeerColumnInventory_nacount

##############################################
#####                          ###############
#####         Question 4       ###############
#####     Calc Median ABV&IBV  ###############
#####   by state and bar chart ###############
##############################################

#Make data frame with only State, ABV, IBU
DF_ABV_IBU <- BeersAndBreweries[,c("StateName","ABV","IBU")]
head(DF_ABV_IBU)

#remove any rows with a NA value using 'complete.cases'
DF_ABV_IBU_noNA <- DF_ABV_IBU[complete.cases(DF_ABV_IBU),]
head(DF_ABV_IBU_noNA)

#Calculate MEDIAN values for ABV&IBU by State
MEDIAN_ABV_IBU_by_State <- aggregate(DF_ABV_IBU_noNA[, 2:3],list(DF_ABV_IBU_noNA$StateName), mean)
head(MEDIAN_ABV_IBU_by_State)

#Rename column names
colnames(MEDIAN_ABV_IBU_by_State)[colnames(MEDIAN_ABV_IBU_by_State)=='Group.1'] <- 'State'
colnames(MEDIAN_ABV_IBU_by_State)[colnames(MEDIAN_ABV_IBU_by_State)=='ABV'] <- 'Median_ABV'
colnames(MEDIAN_ABV_IBU_by_State)[colnames(MEDIAN_ABV_IBU_by_State)=='IBU'] <- 'Median_IBU'

#Check data
head(MEDIAN_ABV_IBU_by_State,10)

#######  Plot MEDIAN ABV By STate   #########
BarPlot_ABV_byState <- ggplot(data=MEDIAN_ABV_IBU_by_State, 
                              aes(x=reorder(State, Median_ABV), 
                                  y=Median_ABV)) +
                                  geom_bar(stat="identity")+
                                  coord_flip() +
                                  labs(x="State",
                                       y="Median ABV",
                                       title = "Median Alchol Content by State")
BarPlot_ABV_byState

#######  Plot MEDIAN IBU By STate   #########
BarPlot_IBU_byState <- ggplot(data=MEDIAN_ABV_IBU_by_State, 
                              aes(x=reorder(State, Median_IBU), 
                                  y=Median_IBU)) +
                              geom_bar(stat="identity")+
                              coord_flip()+
                              labs(x="State",
                                   y="Median_IBU",
                                   title = "Median Bitterness Content by State")
BarPlot_IBU_byState

##############################################
#####                          ###############
#####     Question 5           ###############
##### Calc Min & MAX ABV&IBV   ###############
#####      By state            ###############
##############################################

#Find MAX ABV with State
MAX_ABV_byState <- head(BeersAndBreweries[order(BeersAndBreweries$ABV, na.last = TRUE, decreasing=TRUE),c(11,5)],1)
MAX_ABV_byState

#Find MAX IBU with State, column has missing values
MAX_IBU_byState <- head(BeersAndBreweries[order(BeersAndBreweries$IBU, na.last = TRUE, decreasing=TRUE),c(11,6)],1)
MAX_IBU_byState

##############################################
#####                          ###############
#####     Question 6           ###############
##### Summary of ABV Variable  ###############
#####                          ###############
##############################################

#Summary Stats of the ABV variable
SUMMARY_ABV <- summary(BeersAndBreweries$ABV)

#Show ABV SUmmary
SUMMARY_ABV

##############################################
#####                          ###############
#####     Question 7           ###############
##### Realtionship between     ###############
#####     ABV & IBU            ###############
##############################################

#Merge data with State DB
DF_ABV_IBU_noNA <- merge(DF_ABV_IBU_noNA, StateDB, by="StateName", all = TRUE)

#Remove rows with NA's
DF_ABV_IBU_noNA <- DF_ABV_IBU_noNA[complete.cases(DF_ABV_IBU_noNA),]

#Check merge and top of the file
propmiss(DF_ABV_IBU_noNA)
head(DF_ABV_IBU_noNA)

#Scatter plot ABV vs IBU and color by StateRegion
ABUvsIBU <- qplot(ABV, IBU, 
                  xlab = "ABV (Alcholo Content)",
                  ylab = "IBU (Bitterness)",
                  main= "ABV vs IBU", 
                  colour=StateRegion,
                  data=DF_ABV_IBU_noNA)

#Show Scatter Plot
ABUvsIBU
