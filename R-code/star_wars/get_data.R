library(rvest)
library(rjson)
library(stringdist)

director.urls <- 
  c("JJ Abrams" = "http://www.rottentomatoes.com/celebrity/421669964/",
    "George Miller" = "http://www.rottentomatoes.com/celebrity/1042523-george_miller/",
    "George Lucas" = "http://www.rottentomatoes.com/celebrity/george_lucas/",
    "Steven Spielberg" = "http://www.rottentomatoes.com/celebrity/steven_spielberg/",
    "Michael Bay" = "http://www.rottentomatoes.com/celebrity/162652380/",
    "Francis Lawrence" = "http://www.rottentomatoes.com/celebrity/francis_lawrence/",
    "Ridley Scott" = "http://www.rottentomatoes.com/celebrity/director-ridley-scott",
    "Sam Mendes" = "http://www.rottentomatoes.com/celebrity/sam_mendes",
    "Sam Raimi" = "http://www.rottentomatoes.com/celebrity/sam_raimi",
    "Shawn Levy" = "http://www.rottentomatoes.com/celebrity/1048166-shawn_levy",
    "Quentin Tarantino" = "http://www.rottentomatoes.com/celebrity/quentin_tarantino",
    "Chris Columbus" = "http://www.rottentomatoes.com/celebrity/chris_columbus",
    "James Cameron" = "http://www.rottentomatoes.com/celebrity/james_cameron",
    "Bryan Singer" = "http://www.rottentomatoes.com/celebrity/bryan_singer",
    "Robert Zemeckis" = "http://www.rottentomatoes.com/celebrity/robertzemickis",
    "Bill Condon" = "http://www.rottentomatoes.com/celebrity/bill_condon",
    "Tom McCarthy" = "http://www.rottentomatoes.com/celebrity/thomas_mccarthy",
    "Oliver Stone" = "http://www.rottentomatoes.com/celebrity/oliver_stone/",
    "Paul Feig" =  "http://www.rottentomatoes.com/celebrity/paul_feig",
    "Ivan Reitman" = "http://www.rottentomatoes.com/celebrity/ivan_reitman")
     
director.movies <- data.frame()
i <- 0
for (url in director.urls) {
  i <- i + 1
  html <- read_html(url)
  dat <- html_nodes(html, "#filmographyTbl")
  movies <- html_table(dat)[[1]]  # should be movies rather than tv
  movies$director <- names(director.urls)[i]
  movies$director.writes <- grepl("writer", movies$CREDIT, ignore.case = TRUE)
  movies$director.produces <- grepl("producer", movies$CREDIT, ignore.case = TRUE)
  movies$director.directs <- grepl("director", movies$CREDIT, ignore.case = TRUE)
  movies <- movies[movies$director.directs, ]
  movies <- movies[, c("director", "TITLE", "YEAR", "director.writes", "director.produces")]
  names(movies) <- c("director", "movie", "year", "director.writes", "director.produces")
  director.movies <- rbind(director.movies, movies)
}

# Deal with duplicates by taking max. Results in logical OR of boolean vars.
director.movies <- aggregate(cbind(director.writes, director.produces)~ director + movie + year,
          data=director.movies, max, na.rm=TRUE)

# manual check
table(director.movies$director)
View(director.movies)

director.movies$echo.title <- ""
director.movies$echo.year <- as.numeric(NA)
director.movies$tomato.fresh <- as.numeric(NA)
director.movies$tomato.rotten <- as.numeric(NA)
director.movies$rating <- as.character(NA)
director.movies$genre <- as.character(NA)

# Get Rotten Tomatoes data from OMDB
for (i  in 1:nrow(director.movies)) {
  print(director.movies$movie[i])
  request <- paste0("http://www.omdbapi.com/?t=", director.movies$movie[i],
    "&y=", director.movies$year[i], "&r=json&tomatoes=true")
  json.data <- try(fromJSON(file = URLencode(request)))
  if (!("Error" %in% names(json.data))) {
    director.movies$echo.title[i] <- json.data$Title  #Echo title
    director.movies$echo.year[i] <- as.numeric(json.data$Year)
    director.movies$tomato.fresh[i] <- as.numeric(json.data$tomatoFresh)
    director.movies$tomato.rotten[i] <- as.numeric(json.data$tomatoRotten)
    director.movies$rating[i] <- json.data$Rated
    director.movies$genre[i] <- json.data$Genre
  }
}

# Create variables for cleaning data set
ratings.available <- !is.na(director.movies$tomato.fresh + director.movies$tomato.rotten)
director.movies$title.dist <- stringdist(director.movies$movie,
                                         director.movies$echo.title,
                                         method = "dl")
# Subsetting
director.movies <- director.movies[ratings.available, ]

director.starts <- with(director.movies, aggregate(year, list(director), FUN = "min"))
names(director.starts) <- c("director", "dir.start.year")

director.movies <- merge(director.movies, director.starts)
director.movies$dir.years.exp <- with(director.movies, year - dir.start.year)

#TODO: Manually look at large levenstein distances in title.dist as QC check
to.write <- director.movies[order(director.movies$director, director.movies$year),
                        c("director", "movie", "year", "rating", "genre",
                          "director.writes", "director.produces",
                          "tomato.fresh", "tomato.rotten", "dir.years.exp")]

write.csv(to.write, "C:/Users/User/Documents/statistics/movies.csv",
          row.names = F)

