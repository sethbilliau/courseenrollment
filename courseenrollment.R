# course enrollment
source('styleguide.R')
source('cleaner.R')

# top 10 class
top10.df = dat20[2:11,]

# barplot for top 10 enrollment
p_full <- ggplot(top10.df, aes(x=reorder(Course, Total), y=Total))+
  geom_bar(stat='identity', fill = rev(c(primary,primary))) + 
  labs(title="Top 10 by Total Enrollment: Spring 2020") +
  xlab("Enrollment") +
  ylab("Course") + 
  coord_flip() +
  theme_hodp()
p_full

grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), 
                  width = unit(1.5, 'cm'))


get_classes = function(dat, year) { 
  relevant_classes = c('ECON 10B', "LIFESCI 1B", "PHYSCI 11",
                       'CHEM 27', 'MATH 21B', 'PHYSCI 3', 'COMPSCI 124',
                       'ECON 1010B', 'MATH 21A', 'AFRAMER 119X')
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


make_graph <- function(dat, year) { 
  p_full <- ggplot(dat, aes(x=reorder(Course, Total), y=Total, fill=fill))+
    geom_bar(stat='identity') + 
    scale_fill_manual(values=primary, drop = FALSE) + 
    labs(title=paste("Top 20 by Total Enrollment: Spring", year)) +
    xlab("Enrollment") +
    ylab("Course") +
    #scale_y_continuous(expand = c(0,0)) +
    coord_flip(ylim=c(0,600)) +
    guides(fill=guide_legend(title="Course Type")) + 
    theme_hodp() 
  return(p_full)
}


top20_20 = get_topn(dat20)
top20_19 = get_topn(dat19)
top20_18 = get_topn(dat18)
top20_17 = get_topn(dat17)
top20_16 = get_topn(dat16)
# top20_20$Course <- relevel(top20_20$Course, "i")

top20_16$fill= c(rep("Social Science",2), 
                 rep("Science",2),
                 "GenEd",
                 "Social Science",
                 rep("SEAS",3),
                 rep("Science",2),
                 rep("GenEd",2), 
                 "Social Science", 
                 "GenEd", 
                 rep("SEAS", 3),
                 "Humanities",
                 "Science")
top20_16$fill <- as.factor(top20_16$fill)
top20_16$fill <- factor(top20_16$fill,levels(top20_16$fill)[c(5,3,4,2,1)])

top20_17$fill = c(rep("Social Science",2),
                  "GenEd",
                  "Science",
                  "GenEd",
                  "Social Science",
                  rep("SEAS",3),
                  rep("Science",2),
                  rep("GenEd",2), 
                  "Social Science", 
                  "GenEd", 
                  rep("SEAS", 3),
                  "Humanities",
                  "Science")
top20_17$fill <- as.factor(top20_17$fill)
top20_17$fill <- factor(top20_17$fill,levels(top20_16$fill)[c(1:5)])

top20_18$fill = c(rep("Social Science",2), 
                  "SEAS", 
                  rep("Science",2),
                  'GenEd', 
                  'SEAS',
                  "GenEd",
                  "SEAS",
                  "Science",
                  rep("SEAS",3),
                  "Science",
                  "GenEd",
                  "SEAS",
                  "Social Science",
                  rep("SEAS",3))
top20_18$fill <- as.factor(top20_18$fill)
top20_18$fill <- factor(top20_18$fill,levels(top20_16$fill)[c(1:5)])

top20_19$fill = c("Social Science", 
                  rep("Science",2),
                  "Social Science", 
                  "SEAS", 
                  "Social Science", 
                  "Science",
                  "GenEd", 
                  "Social Science", 
                  "Science",
                  "Science", # History of Science
                  "GenEd",
                  rep("SEAS",2),
                  "Science",
                  "GenEd",
                  "SEAS",
                  "Humanities",
                  "GenEd",
                  "Humanities")
top20_19$fill <- as.factor(top20_19$fill)
top20_19$fill <- factor(top20_19$fill,levels(top20_16$fill)[c(1:5)])

top20_20$fill = c("Social Science", 
                  "Science",
                  "Social Science", 
                  "Science",
                  rep("SEAS", 2),
                  "GenEd", 
                  "Science",
                  "Social Science", 
                  rep("SEAS",2),
                  "GenEd",
                  "Social Science", 
                  "Science",
                  "SEAS",
                  "Humanities",
                  rep("GenEd",4))
top20_20$fill <- as.factor(top20_20$fill)
top20_20$fill <- factor(top20_20$fill,levels(top20_16$fill)[c(1:5)])

p16 = make_graph(top20_16, 2016); p16
p17 = make_graph(top20_17, 2017); p17
p18 = make_graph(top20_18, 2018); p18
p19 = make_graph(top20_19, 2019); p19
p20 = make_graph(top20_20, 2020); p20

