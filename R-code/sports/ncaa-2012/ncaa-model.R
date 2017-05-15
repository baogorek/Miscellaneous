rm(list = ls())

scores <- read.csv("C:\\Documents and Settings\\ogorekb\\My Documents\\March Madness\\ncaa2012.csv", stringsAsFactors = FALSE )
summary(scores)

scores.refine <- subset(scores, visiting.team != ""  & home.team != "")
scores.refine <- subset(scores.refine, visiting.score != 0  & home.score != 0)

scores.refine$v.score <- as.numeric(scores.refine$visiting.score)
scores.refine$h.score <- as.numeric(scores.refine$home.score)

scores.refine <- subset(scores.refine, !is.na(v.score)  & !is.na(h.score) )

scores.refine$visiting.score <- NULL
scores.refine$home.score <- NULL

scores.refine$v.win <- as.numeric((scores.refine$v.score > scores.refine$h.score))
scores.refine$h.win <- as.numeric((scores.refine$v.score < scores.refine$h.score))

common.teams <- intersect(scores.refine$visiting.team, scores.refine$home.team)

scores.refine <- subset(scores.refine, visiting.team %in% common.teams & home.team %in% common.teams)


library(sqldf)
analyze.df <- sqldf(
              "SELECT visiting_team as 'visiting.team', home_team as 'home.team', SUM(v_win) AS 'visitor.wins', SUM(h_win) as 'home.wins'
               FROM 'scores.refine'
               GROUP BY visiting_team, home_team
               ORDER BY visiting_team, home_team
              ")

games.df <- subset(analyze.df, visitor.wins + home.wins > 0)

library(BradleyTerry2)

NCAAModel <- BTm(cbind(visitor.wins, home.wins), visiting.team, home.team, id = "team_", refcat = "Virginia", data = games.df)
summary(NCAAModel)
coef(NCAAModel)
NCAAModel["aic"]


games.df$visiting.team <- data.frame(team_ = games.df$visiting.team, at.home = 0)
games.df$home.team <- data.frame(team_ = games.df$home.team, at.home = 1)

NCAAModel2 <- update(NCAAModel, formula = ~ team_ + at.home)
summary(NCAAModel2)

NCAAModel["aic"]
NCAAModel2["aic"]

write.csv(coef(NCAAModel2), "C:\\Documents and Settings\\ogorekb\\My Documents\\March Madness\\model_picks.csv")