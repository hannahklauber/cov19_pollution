# Figure 1: Spatial and temporal variation in the occurance of inversion episodes
rm(list = ls())

load("data/data_daily.RData")

# summarize daily inversions to the weekly level
data <- data_daily %>% select(GID_2, GID_0, NAME_2, inv_contin_pixel, week) %>%
  group_by(GID_2, week,GID_0, NAME_2,) %>% 
  summarise(inv_contin = sum(inv_contin_pixel, na.rm = T))

# rename cities
data<- data %>% mutate(NAME_2 = ifelse(NAME_2 == "Roma", "Rome", NAME_2),
                       NAME_2 = ifelse(NAME_2 == "Bruxelles", "Brussels", NAME_2),
                       NAME_2 = ifelse(NAME_2 == "Greater London", "London", NAME_2))


# dataframe for plotting
pUrau          <- data %>% group_by(GID_2) %>% mutate(urau_name=as.character(NAME_2)) %>% arrange(week) 
pUrau$urau_fac <- factor(pUrau$urau_name, levels=c(unique(pUrau$urau_name)))

# recode 0 values to NAs
pUrau <- pUrau %>%  mutate(inv_contin = ifelse(inv_contin == 0, NA, inv_contin))

# recode country IDs
pUrau <- pUrau %>% mutate(GID_0 = ifelse(GID_0 == "USA", "US", 
                                         ifelse(GID_0 == "ITA", "IT", 
                                                ifelse(GID_0 == "GBR", "UK", 
                                                       ifelse(GID_0 == "DEU", "DE", 
                                                              ifelse(GID_0 == "BEL", "BE","BR"))))))

# rescale weeks to start at 0
pUrau$week <-pUrau$week-4

# define breaks for the color scale
my_breaks <- c(0, 0.01, 0.1, 1, 10, 100)

# plot the map
gp <- ggplot(pUrau, aes(week,reorder(urau_fac, inv_contin, median, order=TRUE), fill= inv_contin)) + 
  geom_tile() + 
  scale_fill_viridis(option="magma", trans = scales::pseudo_log_trans(sigma = 0.001),
                     direction = -1,na.value = "white", breaks=my_breaks, name = "inversion strength")+
  facet_grid(GID_0 ~ .,switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(100, "lines"))+
  theme_tufte() + 
  theme(axis.line.y=element_line()) +
  ylab("")+xlab("week ")+
  scale_y_discrete(breaks = unique(pUrau$urau_fac[pUrau$urau_fac 
                                                  %in% c("New York", "Berlin", "London", "São Paulo", "Brussels", "Rome")]))

# add cut-off country label for Belgium
pg <- ggplotGrob(gp)

for(i in which(grepl("strip-l-1", pg$layout$name))){
  pg$grobs[[i]]$layout$clip <- "off"
}
grid::grid.draw(pg) 
