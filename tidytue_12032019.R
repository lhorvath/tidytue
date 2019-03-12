library(ggplot2)
library(ggthemes) # the 538 theme is available as template
library(babynames) # census records of names and gender available on cran
library(data.table)

board_games <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv", stringsAsFactors = FALSE)

###### 538 plot ======
# 1/3________________________________________labels to show on plot
board_games$labels <- ifelse(
  board_games$name == 'Acquire', 'Acquire', ifelse(
    board_games$name == 'Mouse Trap', 'Mouse Trap',ifelse(
      board_games$name == 'Connect Four', 'Connect Four', ifelse(
        board_games$name == 'Twilight Struggle', 'Twilight Struggle', NA
      )
    )
  )
)

# 2/3________________________________________points to highlight on plot
emph_dat <- data.frame(
  year = board_games$year_published[!is.na(board_games$labels)],
  rate = board_games$average_rating[!is.na(board_games$labels)],
  label = board_games$labels[!is.na(board_games$labels)]
)

# 3/3________________________________________plot
ggplot(data = board_games, aes(x = year_published, y = average_rating)) + 
  geom_point(alpha = .05, stroke = 0, colour = 'gray29') + 
  geom_smooth(formula = y ~ x^2, method = 'loess', se = FALSE, color = 'orangered', linetype = 2) +
  geom_text(data = board_games, aes(label = labels), na.rm = TRUE, color = '#333333', fontface = 'bold', nudge_y = .6) +
  geom_point(data = emph_dat, mapping = aes(x=year, y=rate)) +
  xlab('') +
  labs(title = 'A Golden Age of Board Games?', subtitle = 'Average user ratings for games by original year of production') +
  ylim(c(0,10)) +
  xlim(c(1950,2010)) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + ylab('Average user rating\n')
  

###### lead creator gender ======

#_________________________________________predict gender w census data
babynames <- as.data.table(babynames)

babynames <- babynames[babynames$year > 1910,.N,by=list(name,sex)][order(name,sex)]
babynames <- dcast(babynames, formula = name ~ sex)

board_games$fnames <- unlist(lapply(board_games[,'artist'], function(x) strsplit(x, ' ')[[1]][1]))

getsex <- function(x) {
  if (x %in% babynames$name) {
    attr(which.max(babynames[which(babynames$name == x)]),'names') 
  } else {
    NA
  }
}

board_games$sex_pred <- unlist(lapply(fnames, function(x) ifelse(is.na(x), NA, getsex(x))))

board_games <- as.data.table(board_games)
board_games <- board_games[!is.na(sex_pred),,]

# descriptive statistics                
board_games[,.N,by=sex_pred]
board_games[,.N,by=category][order(-N)][1:30]

# explored grouping categories but too many possiblities
# board_games[,cat:=unlist(lapply(category, function(x) ifelse(grepl('Wargame',x), 'Wargame', x)))]

# list of top 30 categories
catsel <- board_games[!is.na(cat),.N,by=cat][order(-N)][1:30]$cat

# __________________________________________average ratings plot

plotdat <- board_games[cat %in% catsel,c('average_rating','users_rated','sex_pred','cat'),with=FALSE]

rating <- merge(
  plotdat[,.(avg=mean(average_rating)),by=list(cat,sex_pred)],
  plotdat[,.(allN = sum(average_rating)),by=cat], by = 'cat' # didn't end up using allN to compute a relative measure
)

# sorting levels for plot based on popularity (women creators only)
sort_levels <- rating[sex_pred=='F', c('cat','avg'), with=FALSE][order(avg)]$cat
rating$cat <- factor(rating$cat, levels = sort_levels)

rating$lead <- ifelse(
  rating$sex_pred=='F', 'Lead creator female', 'Lead creator male'
)

ggplot(data = rating, mapping = aes(x = cat, y = avg, group = cat, colour = lead)) + 
  geom_line(colour = '#333333', linetype = 2) +
  geom_point() +
  ylab('Average rating') +
  xlab('') +
  labs(caption = 'Gender predicted based on first name') +
  coord_flip() + 
  scale_color_manual(values = c("orangered","#333333")) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "#333333", fill=NA, size=1),
        text = element_text(colour = "#333333"),
        axis.text.x = element_text(colour = "#333333"),
        axis.text.y = element_text(colour = "#333333"),
        legend.title = element_blank(),
        legend.box = 'horizontal',
        legend.box.just = 'top',
        legend.key = element_rect(fill = NA),
        axis.ticks = element_blank())
