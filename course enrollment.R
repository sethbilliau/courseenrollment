# course enrollment
library(tidyverse)
dat = readxl::read_xlsx("class_enrollment_summary_by_term_1.31.20.xlsx")

### Clean 
# Removefirst few bad rows
dat = dat[-c(1:2),]

# recreate header  
colnames(dat) <- as.character(unlist(dat[1,]))
dat = dat[-1, ]

# as.numeric char columns 
dat[7:14] = apply(dat[7:14], 2, as.numeric)

### analyze 
# sorting by total enrollment
sorted.df = arrange(dat, desc(Total))

# top 10 classes plus the grand total row
sorted.df[1:11,]
