# course enrollment
source('styleguide.R')
source('cleaner.R')
dat = readxl::read_xlsx("class_enrollment_summary_by_term_1.31.20.xlsx")

### Clean 
# Removefirst few bad rows
dat = dat[-c(1:2),]

# recreate header  
colnames(dat) <- as.character(unlist(dat[1,]))
colnames(dat)[1:6] = c("ID", "Course", "LongTitle", "SectionCode", "Department", "Instructor")
dat = dat[-1, ]

# as.numeric char columns 
dat[7:14] = apply(dat[7:14], 2, as.numeric)

# aggregate by course
dat.agg = aggregate(dat$Total, by=list(Course=dat$Course), FUN=sum)
colnames(dat.agg)[2] <- "Total" 

### analyze 
# sorting by total enrollment
grandtotal.df = arrange(dat, desc(Total))
total.df = arrange(dat.agg, desc(Total))

# Grand Totals 
grandtotal.df[1,]


# top 10 class
top10.df = total.df[1:12,]

# barplot for top 10 enrollment
p_full <- ggplot(top10.df, aes(x=reorder(Course, Total), y=Total))+
  geom_bar(stat='identity', fill = rev(c(primary,primary))) + 
  labs(title="Top 10 by Total Enrollment: Spring 2020") +
  xlab("Enrollment") +
  ylab("Course") + 
  coord_flip() +
  theme_hodp()
p_full
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


get_classes = function(dat, year) { 
  relevant_classes = c('ECON 10B', "LIFESCI 1B", "PHYSCI 11",
                       'CHEM 27', 'MATH 21B', 'PHYSCI 3', 'COMPSCI 124',
                       'ECON 1010B','MATH 21A', 'AFRAMER 119X')
  classes = dat[dat$Course %in% relevant_classes,]
  classes$year = year
  return(classes)
}


classes16 = get_classes(dat16, 2016)
classes17 = get_classes(dat17, 2017)
classes18 = get_classes(dat18, 2018)
classes19 = get_classes(dat19, 2019)
classes20 = get_classes(dat20, 2020)

classes = rbind(classes16,
                classes17,
                classes18,
                classes19,
                classes20)

classes

p = ggplot(classes, aes(x=year, y = Total)) + 
  geom_point(aes(color = Course)) +
  geom_line(aes(color = Course), size = 1) + 
  labs(title="Class 5-Year Trajectory") +
  xlab("Enrollment") +
  ylab("Course") + 
  theme_hodp()
p
ggplotly(p)

# Barcharts
get_topn = function(dat, n=20) { 
  df = dat[2:(n+1),]
  return(df)
}

make_graph <- function(dat, year, color_vec) { 
  p_full <- ggplot(dat, aes(x=reorder(Course, Total), y=Total, fill=Course))+
    geom_bar(stat='identity') + 
    scale_fill_manual(values=color_vec) + 
    labs(title=paste("Top 20 by Total Enrollment: Spring", year)) +
    xlab("Enrollment") +
    ylab("Course") + 
    coord_flip() +
    theme_hodp() + 
    theme(legend.position = "none")
  return(p_full)
}


top20_20 = get_topn(dat20)
top20_19 = get_topn(dat19)
top20_18 = get_topn(dat18)
top20_17 = get_topn(dat17)
top20_16 = get_topn(dat16)

socialscience = primary[1]
science = primary[2]
humanities = primary[3]
seas = primary[4]
gened = primary[5]

p16 = make_graph(top20_16, 2016, c(rep(socialscience,2), 
                                   rep(science,2),
                                   gened,
                                   socialscience,
                                   rep(seas,3),
                                   rep(science,2),
                                   rep(gened,2), 
                                   socialscience, 
                                   gened, 
                                   rep(seas, 3),
                                   humanities,
                                   science))

p17 = make_graph(top20_17, 2017)
p18 = make_graph(top20_18, 2018)
p19 = make_graph(top20_19, 2019)
p20 = make_graph(top20_20, 2020)

