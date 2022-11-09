# remove.packages("googlesheets4")
# install.packages("googlesheets4")
library(dplyr)
library(googlesheets4)
googlesheets4::gs4_deauth()
googlesheets4::gs4_auth()
#Read google sheets data into R
x <- read_sheet('14AtoBrDGHnwZjnS-mg2YR7oj7d7zyJEjXLk-4jEoCMw')
x <- x[2:ncol(x)]
x

Karpotsev <- x %>%
   count( `Sergei Karpotsev Trophy (Points leader):`)

McDonald <- x %>%
  count( `Jay McDonald Trophy (Goal scoring leader):`)

Dar <- x %>%
  count( `Jeff Dar Trophy (Best two way player):`)

Ferguson <- x %>%
  count( `Turd Ferguson Trophy (Most PIMs):`)

Stevens <- x %>%
  count( `Scott Stevens Trophy (Best defenseman):`)

Biscuit <- x %>%
  count( `Bojo Biscuit Trophy (Best defensive defenseman):`)

Honcho <- x %>%
  count( `Mike Honcho Trophy (Goalie who allows the fewest goals in the season):`)

McBride <- x %>%
  count( `John McBride Trophy (Best goalie):`)

Jesster <- x %>%
  count( `Ryan Jesster Trophy (Best rookie):`)

Mexico <- x %>%
  count( `Ron Mexico Trophy (League MVP voted by the committee):`)

Khan <- x %>%
  count( `Sarmad Khan Trophy (League MVP voted by the players):`)

Karpotsev[order(-Karpotsev$n),]
McDonald[order(-McDonald$n),]
Dar[order(-Dar$n),]
Ferguson[order(-Ferguson$n),]
Stevens[order(-Stevens$n),]
Biscuit[order(-Biscuit$n),]
Honcho[order(-Honcho$n),]
McBride[order(-McBride$n),]
Jesster[order(-Jesster$n),]
Mexico[order(-Mexico$n),]
Khan[order(-Khan$n),]