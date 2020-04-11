### Cleaner
# Removefirst few bad rows

cleaner = function(dat){ 
  dat = dat[-c(1:2),]
  
  # recreate header  
  colnames(dat) <- as.character(unlist(dat[1,]))
  colnames(dat)[1:6] = c("ID", "Course", "LongTitle", "SectionCode", "Department", "Instructor")
  dat = dat[-1, ]
  
  # as.numeric char columns 
  dat[7:14] = apply(dat[7:14], 2, as.numeric)
  
  # aggregate by course
  dat.agg = aggregate(dat$Total, by=list(Course=dat$Course), FUN=sum)
  # colnames(dat.agg)[2] <- "Total" 
  total.df = arrange(dat.agg, desc(Total))
  return(total.df)
}

cleaner18 = function(dat){ 
  dat = dat[complete.cases(dat), ]
  colnames(dat)[1:6] = c("ID", "Course", "LongTitle", "SectionCode", "Department", "Instructor")
  # as.numeric char columns 
  dat[7:14] = apply(dat[7:14], 2, as.numeric)
  
  # aggregate by course
  dat.agg = aggregate(dat$Total, by=list(Course=dat$Course), FUN=sum)
  # colnames(dat.agg)[2] <- "Total" 
  total.df = arrange(dat.agg, desc(Total))
  return(total.df)
}

dat20 = readxl::read_xlsx("class_enrollment_summary_by_term_2.25.20_0.xlsx")
dat19 = readxl::read_xlsx("pastdata/enrollments19.xlsx")
dat18 = readxl::read_xlsx("pastdata/enrollments18.xlsx")
dat17 = readxl::read_xlsx("pastdata/enrollments17.xlsx")
dat16 = readxl::read_xlsx("pastdata/enrollments16.xlsx")
dat20 = cleaner(dat20)
dat19 = cleaner(dat19)
dat18 = cleaner18(dat18)
dat17 = cleaner18(dat17)
dat16 = cleaner18(dat16)



