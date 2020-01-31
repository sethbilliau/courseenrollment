# course enrollment
library(tidyverse)
dat = readxl::read_xlsx("class_enrollment_summary_by_term_1.31.20.xlsx")

# clean 
dat = dat[-c(1:2),]
colnames(dat) <- as.character(unlist(dat[1,]))
dat = dat[-1, ]
dat[7:14] = apply(dat[7:14], 2, as.numeric)


# sorting code 
sort(dat$Total, decreasing = T)

sorted.df = arrange(dat, desc(Total))


