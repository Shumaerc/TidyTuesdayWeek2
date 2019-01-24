library(tidyverse)
library(lubridate)
library(ggrepel)
library(gganimate)

ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")
glimpse(ratings)
head(ratings)

shows <- c("Twin Peaks",
           "The X-Files",
           "Game of Thrones",
           "Breaking Bad",
           "The Wire",
           "The Sopranos",
           "Sex and the City")

ratings_labled <- ratings %>% 
        filter(title %in% shows) %>% 
        mutate(title = case_when(
                title == "Twin Peaks" & date < as.Date("1995-01-01") ~ "Twin Peaks\n(Original)",
                title == "The X-Files" & date < as.Date("2010-01-01") ~ "The X-Files(Original)",
                TRUE ~ title
        ))

View(ratings_labled)        


labels <- ratings_labled %>% 
        group_by(title) %>% 
        filter(row_number(title) == 1)

ratings %>% 
        mutate(year = year(date)) %>% 
        ggplot(aes(x = date, y = av_rating, size = share)) +
        geom_point(color = "gray", alpha = .8) +
        geom_smooth(method = "lm", se = F, linetype = "dashed", colour = "skyblue4") +
        annotate(geom = "text", x = as.Date("2015-01-01"), y = 8.2, 
                 label = "TV drama trend",
                 col = "skyblue4", fontface = "bold", size = 4) +
        geom_point(data = ratings_labled, aes(x = date, y = av_rating, group = title, size = share),
                   colour = "skyblue", inherit.aes = F) +
        geom_line(data = ratings_labled, aes(x = date, y = av_rating, group = title),
                  colour = "blue", inherit.aes = F) +
        geom_text_repel(data = labels, aes(x = date, y = av_rating, label = title),
                        colour = "blue", inherit.aes = F, nudge_y = -.15,
                        fontface = "bold", size = rel(6)) +
        coord_cartesian(ylim = c(5.5, 9.5)) +
        scale_y_continuous(breaks = seq(5.5, 9.5, .5), position = "right") +
        scale_x_date(breaks = as.Date(c("1990-01-01",
                                        "1995-01-01",
                                        "2000-01-01",
                                        "2005-01-01",
                                        "2010-01-01",
                                        "2015-01-01",
                                        "2018-01-01")),date_labels = "%Y") +
        scale_size_continuous(range = c(2,10)) +
        labs(title = "TV dramas shown in America",
             subtitle = "Average IMDb user, ratings, by show and season",
             caption = "Seasons with at least 100 ratings on average") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = .5, size = rel(2.5)),
              plot.subtitle = element_text(hjust = .5, size = rel(1.3), face = "italic"),
              plot.caption = element_text(size = rel(1), face = "italic", colour = "grey60"),
              axis.title = element_blank(),
              axis.text = element_text(size = rel(1.2)),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              legend.position = "none")

