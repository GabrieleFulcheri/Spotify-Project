library(tidyverse)
library(readr)
library(car)
library(gpairs)
library(corrplot)
library(coefplot)
library(lmtest)



songs_normalize <- read_csv("songs_normalize.csv")
View(songs_normalize)

## PART 1: brief data cleaning and preparation

summary(songs_normalize)  ##even if they appear in a top tier list, some songs seam to have a popularity score of 0

songs_normalize %>% select(1:18) %>% filter(popularity == 0) %>% arrange(year)%>% View()## it is weird that very popular songs like Danza Kuduro have a popularity value of 0. This songs are not the majority of cases, therefore they'll be excluded from our model

songs_normalize %>% duplicated()%>% sum() ## hence, we need to select distinct values

sg <- distinct(songs_normalize)
View(sg)

sg %>% distinct(genre) %>% View()  ## it appears to be a genre called "set()", which is clearly an input error
sg %>% select(1:18)%>% filter(genre == 'set()') %>% View() ## since there are onyl 22 entries, we'll exclude them

sg_cleaned <- sg %>% select(1:18) %>% filter(genre!= 'set()' & popularity != 0)
View(sg_cleaned)

## PART 2: Linear Regression

## 2.1
## first of all, we select the values we want to consider in our regression and we test their fit

s1 <- sg_cleaned %>% select(3, 6:17) ## to select only values for regression
View(s1)
unlist(s1)   
scatterplotMatrix(formula = ~ popularity + danceability + energy + loudness + speechiness + valence+tempo, transform=TRUE, by.group= TRUE, data = s1)
corrplot.mixed(cor(s1), upper= "ellipse", tl.pos = "lt", order="AOE")

##since the values' distribution among the variables is, in most cases, a little bit skewed. we need to check for a possible Box-Cox Trasformation to obtain a more normal distribution
powerTransform(s1$popularity)
powerTransform(s1$speechiness)
powerTransform(s1$energy)
powerTransform(s1$danceability)
powerTransform(s1$loudness)
powerTransform(s1$tempo)
powerTransform(s1$valence)  ##as we see, since valence is already normally distributed, powerTranform() returns a value near to 1

lambda_popularity <- coef(powerTransform(s1$popularity))
bcPower(s1$popularity, lambda_popularity)

lambda_speechiness <- coef(powerTransform(s1$speechiness))
bcPower(s1$speechiness, lambda_speechiness)

lambda_energy <- coef(powerTransform(s1$energy))
bcPower(s1$energy, lambda_energy)

lambda_danceability <- coef(powerTransform(s1$danceability))
bcPower(s1$danceability, lambda_danceability)

lambda_tempo <- coef(powerTransform(s1$tempo))
bcPower(s1$tempo, lambda_tempo)

##box-cox transformation for the loudness variable is different since we have negative values. However, as Box and Cox showed, we just need to add an appropriate lambda2 values to y in order to avoid having negative values

summary(s1$loudness)
S1 <- s1 %>% mutate(loudness_2.0 = loudness + 21)
View(S1)

lambda_loudness <- coef(powerTransform(S1$loudness_2.0))
bcPower(S1$loudness_2.0, lambda_loudness)


## Let's check for the correlation now

s2 <- s1 %>% mutate(popularity_trfm = bcPower(s1$popularity,lambda_popularity), 
                    speechiness_trfm = bcPower(s1$speechiness, lambda_speechiness),
                    danceability_trfm = bcPower(s1$danceability, lambda_danceability),
                    energy_trfm = bcPower(s1$energy, lambda_energy),
                    tempo_trfm = bcPower(s1$tempo, lambda_tempo),
                    loudness_trfm = bcPower(S1$loudness_2.0, lambda_loudness)) %>% select(1, 5, 7, 9, 10,12, 14:19)
View(s2)

scatterplotMatrix(formula = ~ popularity_trfm + danceability_trfm + energy_trfm + loudness_trfm + speechiness_trfm + valence+ tempo_trfm, transform=TRUE, by.group= TRUE, data = s2)
corrplot.mixed(cor(s2), upper= "ellipse", tl.pos = "lt", order="AOE")

## as we can see, there is a very weak correlation between popularity and a single other variables considered

##2.2
## let's see if we can predict popularity with a multilinear regression


m1 <- lm(popularity_trfm ~ energy_trfm+loudness_trfm+valence+danceability_trfm+speechiness_trfm, data=s2)
summary(m1)
plot(m1)

m1.2 <- lm(popularity_trfm ~ energy_trfm+loudness_trfm+valence, data=s2)
summary(m1.2)
plot(m1.2)

## 2.3 Multilinear regression among genres - 2nd attempt to predict popularity

sg_cleaned %>% distinct(genre) %>% as.data.frame() %>% print()

## extract a string for creating a column with primary genre

w <- s2 %>% mutate(genre = sg_cleaned$genre) %>% 
            separate(genre, c("main genre", "secondary genre", "tertiary genre"), ",")

table(w$`main genre`)
ggplot(data=w)+ aes(w$`main genre`)+
  geom_bar(mapping=aes(fill=w$`main genre`))+
  theme_classic()+
  labs(x="Main Genres", y="Number of top-position songs", title="Genres' popularity breakdown")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## let's try a linear model on Dance Electronic songs

genre_1 <- w %>% filter(`main genre` == "Dance/Electronic")%>% as.data.frame()

M1 <- lm(popularity_trfm ~ energy_trfm + danceability_trfm+ loudness_trfm + speechiness_trfm, data=genre_1)
summary(M1)

M2 <- lm(popularity ~ +loudness+danceability, data=x)
summary(M2)

genre_2 <- w %>% filter(`main genre` == "hip hop")%>% as.data.frame()

M3 <- lm(popularity_trfm ~ energy_trfm +tempo_trfm+ instrumentalness+ speechiness_trfm, data=genre_2)
summary(M3)

M3.1 <- lm(popularity_trfm ~ tempo_trfm, data=genre_2)
summary(M3.1)  ##best regression so far. Tempo seems to have a modest correlation with the popularity of hip hop songs

corrplot.mixed(cor(genre_2[1:12]), upper= "ellipse", tl.pos = "lt", order="AOE")


genre_3 <- w %>% filter(`main genre` == "pop")%>% as.data.frame()

M4 <- lm(popularity_trfm ~  energy_trfm+ valence +tempo_trfm+ instrumentalness+ speechiness_trfm, data=genre_3)
summary(M4)

M4.1 <- lm(popularity_trfm ~ energy_trfm + valence + instrumentalness+ speechiness_trfm, data=genre_3)
summary(M4.1)

M4.2 <- lm(popularity_trfm ~ energy_trfm, data = genre_3)
summary(M4.2)

genre_4 <- w %>% filter(`main genre` == "metal")%>% as.data.frame()
View(genre_4)

M5 <- lm(popularity_trfm ~ energy_trfm +instrumentalness + loudness_trfm+tempo_trfm, data=genre_4)
summary(M5) ##best model so far, better than m3.1
plot(M5) ##however, residual vs leverage plot shows the limit of this model, due to the small number of observations

##notice however that metal has only 9 observations

genre_5 <- w %>% filter(`main genre` == "rock")%>% as.data.frame()
M6 <- lm(popularity_trfm ~ energy_trfm +instrumentalness + loudness_trfm+tempo_trfm+ speechiness_trfm, data=genre_5)
summary(M6) 

M6.1 <- lm(popularity_trfm ~ instrumentalness+speechiness_trfm, data=genre_5)
summary(M6.1)
plot(M6.1)

## eliminating outliers from M6.1

genre_5.cleaned <- genre_5[-c(41, 43),]
M6.1_cleaned <- lm(popularity_trfm ~ instrumentalness+speechiness_trfm, data=genre_5.cleaned)
summary(M6.1_cleaned)
plot(M6.1_cleaned)

genre_5.cleaned.v2 <- genre_5[-c(41, 43, 99),]
M6.1_cleaned.v2 <- lm(popularity_trfm ~ instrumentalness+speechiness_trfm, data=genre_5.cleaned.v2)
summary(M6.1_cleaned.v2)
plot(M6.1_cleaned.v2)  ##best model so far

cor(genre_5.cleaned.v2$popularity_trfm, genre_5.cleaned.v2$instrumentalness, method = 'pearson')
cor(genre_5.cleaned.v2$popularity_trfm, genre_5.cleaned.v2$speechiness_trfm, method = 'pearson')
corrplot.mixed(cor(genre_5.cleaned.v2[1:12]), upper= "ellipse", tl.pos = "lt", order="AOE")

##let's test now the assumption of the linear model for the only valuable model we have built so far, namely the last one

bptest(M6.1_cleaned.v2)  ## so there is homoscedasticity/costant variance
shapiro.test(genre_5.cleaned.v2$instrumentalness)
shapiro.test(genre_5.cleaned.v2$speechiness_trfm)  ##the hypothesis of normal distribution if the independent variables is violated
