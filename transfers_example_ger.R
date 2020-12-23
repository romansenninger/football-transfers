# ----- load packages 
library("tidyverse")
library("stringr")
library("ggimage")

# ----- load data files 
ger16 <- read.csv("/1-bundesliga2016.csv")
ger17 <- read.csv("/1-bundesliga2017.csv")
ger18 <- read.csv("/1-bundesliga2018.csv")
ger19 <- read.csv("/1-bundesliga2019.csv")

# ----- merge data files 
ger <- rbind(ger16, ger17, ger18, ger19)

# ----- clean data and remove youth team transfers 
ger$Club <- as.character(ger$Club)
ger$ClubInvolved <- as.character(ger$ClubInvolved)

ger$Club[ger$Club == "1. FC Köln"] <- "1.FC Köln"
ger$ClubInvolved[ger$ClubInvolved == "RasenBallsport Leipzig U19"] <- "RB Leipzig U19"
ger$ClubInvolved[ger$ClubInvolved == "RasenBallsport Leipzig II"] <- "RB Leipzig U19"

ger$youthteam <- str_detect(ger$ClubInvolved, ger$Club, negate = F)

# ----- remove end of loan
ger$Fee <- as.character(ger$Fee)
ger$loan <- str_detect(ger$Fee, "End of", negate = F)

ger <- subset(ger, loan == FALSE)

# ----- create count data set bundesliga
count <- ger %>% group_by(Club) %>%
  count(ClubInvolved)

count <- count[with(count,order(-n)),]

count <- count[1:5,]

count <- count %>% mutate(
  image = case_when(
    ClubInvolved == "Red Bull Salzburg" ~ "/logos/rbs.png",
    ClubInvolved == "VfB Stuttgart" ~ "/logos/vfb.png", 
    ClubInvolved == "TSG 1899 Hoffenheim" ~ "/logos/hoffenheim.png",
  )
)

count <- count %>% mutate(
  image2 = case_when(
    Club == "RB Leipzig" ~ "/logos/leipzig.png",
    Club == "Bayer 04 Leverkusen" ~ "/logos/bayer.png",
    Club == "Bayern Munich" ~ "/logos/fcb.png",
    Club == "SC Freiburg" ~ "/logos/freiburg.png",
    Club == "Fortuna Düsseldorf" ~ "/logos/fortuna.png",
    
    
  )
)

count <- count %>% mutate(
  image3 = case_when(
    ClubInvolved == "Red Bull Salzburg" ~ "/logos/pfeil.png",
    ClubInvolved == "VfB Stuttgart" ~ "/logos/pfeil.png", 
    ClubInvolved == "TSG 1899 Hoffenheim" ~ "/logos/pfeil.png",
  )
)

# ----- size parameter
n <- count$n

# ----- make figure
ggplot(count, aes(x=n, y = reorder(Club, n))) + 
  geom_segment( aes(y = reorder(Club, n), yend=Club, x=-0.5, xend=n+0.7), color="white") +
  geom_image(aes(image = image), size = 0.025*n) + 
  geom_image(x=-1, aes(image = image2), size = 0.025*n) +
  geom_image(x=2, aes(image = image3), size = 0.025*n) +
  theme_minimal() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(face="bold", color="#993333", size=16),
        plot.title = element_text(face="bold", color="#993333", size=13)) +
  ggtitle("Bundesliga - Number of players from same club, 2016-2019") +
  annotate("text", x = 4.5, y="RB Leipzig", label = "7 transfers in 4 seasons. \nAverage age: 20 years. \nLeipzig loves Salzburg's talent!",
           size = 2.5, fontface="bold", color="#993333") +
  annotate("text", x = 6, y="Bayer 04 Leverkusen", label = "...7 other clubs have signed three \nplayers from the same club. \n(e.g. Borussia Dortmund from FC Barcelona)",
           size = 2.5, fontface="bold", color="#993333") +
  annotate("text", x = 6.6, y="SC Freiburg", label = "\n\nPopular TSG Hoffenheim! \nA total of 7 clubs have \ntransferred players from \nTSG Hoffenheim more than once.",
           size = 2.5, fontface="bold", color="#993333") + xlim(-1.5,8)
ggsave("/figures/ger.png", widt = 16, height = 12, units = "cm", dpi = 350)

