library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)

titles=read.csv(file = 'C:\\Users\\KINTA\\Downloads\\netflix_shows_movies.csv', na.strings = c("NA", ""), stringsAsFactors=F)
head(titles)

titles = subset(titles, select = -c(show_id) )

data.frame("variable"=c(colnames(titles)), "missing values count"=sapply(titles, function(x) sum(is.na(x))), row.names=NULL)

#function to find a mode
getmode <- function(v) {
	uniqv <- unique(v)
	uniqv[which.max(tabulate(match(v, uniqv)))]
}
titles$rating[is.na(titles$rating)] <- getmode(titles$rating)
titles$date_added <- as.Date(titles$date_added, format = "%B %d, %Y")

#drop duplicated rows based on the title, country, type and release_year
titles=distinct(titles,title,country,type,release_year, .keep_all= TRUE)

# top 20 genres on netflix 
s3 <- strsplit(titles$listed_in, split = ", ")
titles_listed_in <- data.frame(type = rep(titles$type, sapply(s3, length)), listed_in = unlist(s3))
titles_listed_in$listed_in <- as.character(gsub(",","",titles_listed_in$listed_in))

df_by_listed_in_full <- titles_listed_in %>% group_by(listed_in) %>% summarise(count = n()) %>%
	arrange(desc(count)) %>% top_n(20)

fig1 <- plot_ly(df_by_listed_in_full, x = ~listed_in, y = ~count, type = 'bar', marker = list(color = '#aaaaaa'))
fig1 <- fig1 %>% layout(xaxis=list(categoryorder = "array", categoryarray = df_by_listed_in_full$listed_in, title="Genre"), yaxis = list(title = 'Count'), title="20 Top Genres On Netflix")

fig1

# rating distribution
df_by_rating_only_full <- titles %>% group_by(rating) %>% summarise(count = n())
fig2 <- plot_ly(df_by_rating_only_full, labels = ~rating, values = ~count, type = 'pie')
fig2 <- fig2 %>% layout(title = 'Amount Of Content By Rating',
												xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
												yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig2


