library(tidyverse)
library(readxl)
library(lubridate)
#read in data
data <- read_xlsx("../../Data/Vog and Resp Meds sample data .xlsx")

#rename data for easy manipulation

data <- data %>% 
  rename(prod_name = `Product Name`,
         date = `Year-Month-Week`,
         location = `Zip/Territory`)

data %>% 
  mutate(date = dmy(date))


#Data Dictionary:

#prod_name: type of drug, more specific than "class"
#NDC: National drug code, not very important for me, but possibly for reference
#date: format is YYYYMMDD
#Year, Month, Day are all what they are
#location: zip code or region, region is a aggregation of multiple zip codes if a
#zip code did not have at least 3 pharmacies within it, due to data privacy laws
#County is the county where the purchase was made, and corresponds to which island

#TRx: Number of prescriptions

#Class: A more specific description of the drug purchased. 

# Look at all of the classes independently, do the same comparisons 

#hypothesis: See if the data shows that people use more respiratory med during
#times of high, medium, and low

#second hypothesis: difference is going to be more pronounced in antihiasmatic
#and broncho



# Get to know the data

summary(as.factor(data$prod_name))
