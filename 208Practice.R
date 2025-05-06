dat = read.csv("battingbowling.csv")
all_r = dat[which((dat$Bowling <= 40) & (dat$Batting >= 25)),]
tab = table(all_r$Team)
names(which.min(tab))

fx = function(n){
  return((1 + 1/n)^n)
}

fn = fx(1:1e1)

plot(x = 1:1e1, y = 1:1e1, type = "l")
abline(a = 1, b = 0.5)

library(rvest)
html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
dir <- html %>%  
html_elements(".info.director")
all_dirs <- sapply(dir, function(a) a %>% html_elements("a") %>% html_text())
all_dirs <- sapply(all_dirs, paste, collapse = ", ")

data(mtcars)
by_cyl = mtcars %>% group_by(cyl)
summarise(mtcars, mpg = mean(mpg))
by_ = mtcars %>% group_by(m = gear)
by_wt = mtcars %>% group_by(weight = cut(wt, 3))
by_wt = by_wt %>% relocate(weight, .before = wt)

html <- read_html("https://www.relianceiccrankings.com/ranking/womenodi/batting/")
dat <- html %>% html_table()
batting <- dat[[1]]
colnames(batting)[4] <- "Country"
colnames(batting)[1] <- "Ranking"


country <- html %>% html_elements("tr img") %>% html_attr("alt")


batting$Country<- country

india <- batting %>% filter(Country == "IND")

india %>% select(Ranking, Name, Country)
india$Ranking

india %>% summarise(n = n())

by_country <- batting %>% group_by(Country)
by_country %>% summarize(number = n())

mean_Rank <- by_country %>% summarise(average.rank = mean(Ranking))
mean_Rank %>% arrange(average.rank)

hist(mtcars$wt, breaks=5, col = "blue")
hist(mtcars$drat, breaks=5, col = adjustcolor("red", alpha.f = 0.1))

library(ggplot2)

load("covid.RData")
names(india_covid) <- c("state", "confirmed", "active", "cured", "death")
covid <- as_tibble(india_covid)

covid <- covid[order(covid$confirmed), ]
covid$state <- factor(covid$state, levels = covid$state)
covid$rate <- round(covid$death/covid$confirmed, 3)


g <- ggplot(covid, aes(x = state, y = log10(confirmed)))
g + geom_bar(stat = 'identity', aes(fill = rate)) + 
  labs(y = "Log of Confirmed Cases", x = "State/UT", 
       title = "Covid India Data", subtitle = "Confirmed Cases shaded by rate of death") + 
  coord_flip()

covid <- covid[order(covid$confirmed), ]

g <- ggplot(covid, aes(x = state, y = log10(confirmed), label = rate))
g +  geom_point(stat='identity', fill="black", size=8) + 
  geom_segment(aes(y = 0, x = state, yend = log10(confirmed), xend = state), color = "black") +
  geom_text(color="white", size=2) + 
  labs(y = "Log of Confirmed Cases", x = "State/UT", 
       title = "Covid India Data", subtitle = "Confirmed Cases with rate as text") + 
  coord_flip()

library(Rcpp)
sourceCpp("Trial_R208.cpp")
