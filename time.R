library(plyr)
library(ggplot2)
library(scales) 
library(reshape2)
library(dplyr)

# setwd("c:/Users/dhadley/Documents/GitHub/tcovers")
setwd("/Users/dphnrome/Documents/Git/tcovers")

#### d ####
d <- read.csv("./data/TimeCoverData.csv")



#### Visualize ####


my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=9), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=15,  hjust=0) # Enlarge, left-align title
    #,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )



#### dplr ####
top <- d %>%
  group_by(Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 9)

ggplot(top, aes(x=reorder(Name, count), y=count)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Time Covers") + xlab(NULL) +
  ylab("Time Covers As Of Dec '14") + 
  geom_text(aes(label = count), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers.png", dpi=300, width=5, height=5)



top <- d %>%
  filter(Topic == "Religion") %>%
  group_by(Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 1)

ggplot(top, aes(x=reorder(Name, count), y=count)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Time Covers, Religious Figures") + xlab(NULL) +
  ylab("Time Covers As Of Dec '14") + 
  geom_text(aes(label = count), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers2.png", dpi=300, width=5, height=5)




top <- d %>%
  filter(Topic == "Science") %>%
  group_by(Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 1)

ggplot(top, aes(x=reorder(Name, count), y=count)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Time Covers, Scientists") + xlab(NULL) +
  ylab("Time Covers As Of Dec '14") + 
  geom_text(aes(label = count), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers3.png", dpi=300, width=5, height=5)



top <- d %>%
  filter(Topic == "Entertainment") %>%
  group_by(Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 1)

ggplot(top, aes(x=reorder(Name, count), y=count)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Time Covers, Entertainers") + xlab(NULL) +
  ylab("Time Covers As Of Dec '14") + 
  geom_text(aes(label = count), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers4.png", dpi=300, width=5, height=5)



top <- d %>%
  group_by(Last.Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 11)

ggplot(top, aes(x=reorder(Last.Name, count), y=count)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Time Covers By Last Name") + xlab(NULL) +
  ylab("Time Covers As Of Dec '14") + 
  geom_text(aes(label = count), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers5.png", dpi=300, width=5, height=5)




# https://gist.github.com/mrdwab/10744946
dcastMult <- function(data, formula, value.var = "value", 
                      funs = list("min" = min, "max" = max)) {
  require(reshape2)
  if (is.null(names(funs)) | any(names(funs) == "")) stop("funs must be named")
  Form <- formula(formula)
  LHS <- as.character(Form[[2]])
  if (length(LHS) > 1) LHS <- LHS[-1]
  temp <- lapply(seq_along(funs), function(Z) {
    T1 <- dcast(data, Form, value.var = value.var, 
                fun.aggregate=match.fun(funs[[Z]]), fill = 0)
    Names <- !names(T1) %in% LHS
    names(T1)[Names] <- paste(names(T1)[Names], names(funs)[[Z]], sep = "_")
    T1
  })
  Reduce(function(x, y) merge(x, y), temp)
}


top <- d %>%
  mutate(tab = 1) %>%
  melt(id=c("Name"), measure=c("Year")) %>%
  dcastMult(Name ~ variable, funs = list("min" = min, "max" = max, "min^2" = function(x) min(x)^2)) %>%
  mutate(InBetween = Year_max - Year_min) %>%
  arrange(desc(InBetween)) %>%
  filter(InBetween > 35)

ggplot(top, aes(x=reorder(Name, InBetween), y=InBetween)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Time In Between Time Covers") + xlab(NULL) +
  ylab("Years Between First and Last Cover") + 
  geom_text(aes(label = InBetween), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers6.png", dpi=300, width=5, height=5)  



#### Now combine them ####
topYears <- d %>%
  melt(id=c("Name"), measure=c("Year")) %>%
  dcastMult(Name ~ variable, funs = list("min" = min, "max" = max, "95thPerc" = function(x) quantile(x, c(.95)))) %>%
  mutate(InBetween = Year_max - Year_min) %>%
  mutate(YearsOfCoverage = 2014 - Year_min)

topCovers <- d %>%
  group_by(Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


topAll <- merge(topCovers, topYears, by="Name")


final <- topAll %>%
  mutate(Normalized = count / YearsOfCoverage) %>%
  mutate(Normalized2 = count / (Year_max - Year_min)) %>%
  filter(count > 1) %>%
  arrange(desc(Normalized)) %>%
  filter(Normalized > .3)


ggplot(final, aes(x=reorder(Name, Normalized), y=Normalized)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Covers Normalized") + xlab(NULL) +
  ylab("# Of Covers Normalized by Years Covered (2014 - First Cover)") + 
  geom_text(aes(label = round(Normalized, digits = 1)), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers7.png", dpi=300, width=5, height=5)  



final <- topAll %>%
  mutate(Normalized = count / YearsOfCoverage) %>%
  mutate(Normalized2 = count / (Year_max - Year_min)) %>%
  filter(count > 1) %>%
  arrange(desc(Normalized2)) %>%
  filter(count > 5) %>%
  filter(Normalized2 > .4)


ggplot(final, aes(x=reorder(Name, Normalized2), y=Normalized2)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Covers Normalized") + xlab(NULL) +
  ylab("# Of Covers Normalized by Years Covered (Last Cover - First Cover)") + 
  geom_text(aes(label = round(Normalized2, digits = 1)), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers8.png", dpi=300, width=5, height=5) 



final <- topAll %>%
  mutate(Normalized = count / YearsOfCoverage) %>%
  mutate(Normalized2 = count / (Year_max - Year_min)) %>%
  mutate(Normalized3 = count / (Year_95thPerc - Year_min)) %>%
  filter(count > 1) %>%
  arrange(desc(Normalized2)) %>%
  filter(count > 5) %>%
  filter(Normalized2 > .4)


ggplot(final, aes(x=reorder(Name, Normalized3), y=Normalized3)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Covers Normalized") + xlab(NULL) +
  ylab("# Of Covers Normalized by Years Covered (95th Percentile - First Cover)") + 
  geom_text(aes(label = round(Normalized3, digits = 1)), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers9.png", dpi=300, width=5, height=5) 




final <- topAll %>%
  filter(count > 1) %>%
  filter(Year_min > 1989) %>%
  arrange(desc(count)) %>%
  filter(count > 2)


ggplot(final, aes(x=reorder(Name, count), y=count)) + 
  geom_bar(colour="white", fill="Red") + 
  my.theme + ggtitle("Most Time Covers, 1990 On") + xlab(NULL) +
  ylab("# Of Covers") + 
  xlab("People Whose First Cover Was Later Than 1989") + 
  geom_text(aes(label = count), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/covers10.png", dpi=300, width=5, height=5)  

  
