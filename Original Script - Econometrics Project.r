#install.packages("tidyverse")
library(tidyverse)
#install.packages("tidytext")
library(tidytext)
#install.packages("fastDummies")
library(fastDummies)
library(dplyr)
library(DescTools)
library(ggplot2)
library(stargazer)
library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("pscl")

library("ucminf")
library("ordinal")
library("reshape")

library("generalhoslem")
library("oglmx")
library("aod")
#install.packages("brant")
library("brant")
#install.packages("corrplot")
library(corrplot)
#install.packages("texreg")
library(texreg)
library(car)


#Reading data

df = read.csv(file="data/All_Streaming_Shows.csv", header=TRUE)

head(df)
summary(df)
df$IMDB.Rating<-as.numeric(df$IMDB.Rating)
df$R.Rating<-as.numeric(df$R.Rating)
#Cleaning dataset
#Handling missing values in dependent variable & improper values
print(sum(is.na(df$IMDB.Rating)))

count(df%>% filter(R.Rating!=-1 & Description!="-1" & Genre!="-1" & Streaming.Platform!="-1"))

df<-df%>% filter(R.Rating!=-1 & Description!="-1" & Genre!="-1" & Streaming.Platform!="-1")
df<-df%>% filter(R.Rating!=-1)

getmode <- function(v) {
  v <- na.omit(v)  # Usuń NA
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
df <- df %>%
  group_by(R.Rating) %>%
  mutate(IMDB.Rating = ifelse(is.na(IMDB.Rating), getmode(IMDB.Rating), IMDB.Rating)) %>%
  ungroup()

df<-df%>%
  na.omit(IMDB.Rating)

#Genre variable
print(unique(df$Genre))


all_platforms <- unlist(strsplit(df$Streaming.Platform, ","))
all_platforms<-unique(all_platforms)
print(all_platforms)
remove_platforms <- function(input_str) {
  str_replace_all(input_str, paste(all_platforms, collapse="|"), "")
}

df<-df%>% mutate(Genre=sapply(Genre, remove_platforms))
df <- df %>%
  mutate(Genre = str_remove_all(Genre, "\\b\\d{4}\\b"))
df <- df %>%
  mutate(Genre = str_remove_all(Genre, "\\+"))
print(unique(df$Genre))
df<- df %>%
  mutate(Genre = if_else(Genre == "-1", "Unknown", Genre))
df$Genre<- gsub("^,|(?<=,)$", "", df$Genre, perl = TRUE)
df$Genre<-gsub("^,|,$", "",df$Genre)
df$Genre <- str_trim(df$Genre, side = "both")
df<- df %>%
  mutate(Genre = if_else(Genre == "", "Unknown", Genre))
df<- df %>%
  mutate(Genre = if_else(Genre == " ", "Unknown", Genre))
df <- dummy_cols(df, select_columns=c("Genre"), split=",")
df<-subset(df, select=-Genre)

#Content Rating variable
print(unique(df$Content.Rating))
df <- df %>%
  mutate(Content.Rating= str_remove_all(Content.Rating, "\\+"))
df<- df %>%
  mutate(Content.Rating = if_else(Content.Rating=="N/A", "0", Content.Rating))
df<- df %>%
  mutate(Content.Rating = if_else(Content.Rating=="all", "0", Content.Rating))
#df$Content.Rating <- factor(df$Content.Rating, 
#                    levels = c("0", "7", "13", "16", "18"), 
#                    ordered = TRUE)


df<-df%>%
  mutate((Content.Rating = if_else(Content.Rating=="18", "R Rated", "Not R Rated")))
df <- dummy_cols(df, select_columns=c("(...)"))
df<-subset(df, select=-Content.Rating)

#df$Content.Rating=as.numeric(df$Content.Rating)
#print(unique(df$Content.Rating))
#df<- df %>%
#  mutate(Content.Rating = if_else(is.na(Content.Rating), 0, Content.Rating))

#Seasons variable
print(unique(df$No.of.Seasons))
df<-df%>%
  filter(No.of.Seasons!="-1")
df<-df %>% mutate(No.of.Seasons=gsub("\\D", "", No.of.Seasons))
df$No.of.Seasons<-as.numeric(df$No.of.Seasons)


#Streaming Platform
print(unique(df$Streaming.Platform))

count_platforms <- function(x) {
  length(strsplit(x, ",")[[1]])
}

platform_counts <- sapply(df$Streaming.Platform, count_platforms)

df <- df %>% mutate(Platform.Count=platform_counts)
#df$Content.Rating<-as.numeric(df$Content.Rating)
correlation <- cor(df$Genre_Reality, df$Genre_Documentary, method = "spearman")
print(correlation)
#Deleting duplicates

df<-distinct(df)

#no missing values
colSums(is.na(df))

#Description column

custom_stop_words <- tibble(word = c("movie", "about", "with", "will", "that", "leave", "episodes", "imdb", "season", "seasons", "hulu", "prime"))

# Function to extract and count bigrams and trigrams
extract_ngrams <- function(df, n) {
  ngrams <- df %>%
    unnest_tokens(ngram, Description, token = "ngrams", n = n) %>%
    separate(ngram, into = paste0("word", 1:n), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !ifelse(n == 3, word3 %in% stop_words$word, FALSE)) %>%
    filter(!word1 %in% custom_stop_words$word,
           !word2 %in% custom_stop_words$word,
           !ifelse(n == 3, word3 %in% custom_stop_words$word, FALSE)) %>%
    unite(ngram, paste0("word", 1:n), sep = " ") %>%
    count(ngram, sort = TRUE)
  
  return(ngrams)
}


#true crime
#food network - there is genre "food"
#world war
#award winning
#serial killer
#emmy award

# Extract and count bigrams
bigrams <- extract_ngrams(df, 2)
print("Most common bigrams:")
print(bigrams)



# Extract and count trigrams
trigrams <- extract_ngrams(df, 3)
print("Most common trigrams:")
print(trigrams)

# Extract and count trigrams
trigrams <- extract_ngrams(df, 4)
print("Most common trigrams:")
print(trigrams)

keywords <- df %>%
  unnest_tokens(word, Description) %>%
  anti_join(stop_words) %>%
  filter(!word %in% custom_stop_words$word) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(!str_detect(word, "[:punct:]"))


word_frequencies <- keywords %>%
  count(word, sort = TRUE)

#featuring
#family? - it's also present for example in Breaking Bad description, so it can be misleading 
#love
#school   819 ???
print(word_frequencies)

common_phrases <- c("true crime", "food network", "world war","award winning", "serial killer", "emmy award", "featuring", "family", "love")
df$Description <- iconv(df$Description, "UTF-8", "ASCII", sub="") 
df <- df %>%
  mutate(
    true_crime = ifelse(grepl(common_phrases[1], Description, ignore.case = TRUE), 1, 0),
    world_war = ifelse(grepl(common_phrases[3], Description, ignore.case = TRUE), 1, 0),
    award_winning = ifelse(grepl(common_phrases[4], Description, ignore.case = TRUE), 1, 0),
    serial_killer= ifelse(grepl(common_phrases[5], Description, ignore.case = TRUE), 1, 0),
    emmy_award = ifelse(grepl(common_phrases[6], Description, ignore.case = TRUE), 1, 0),
    featuring = ifelse(grepl(common_phrases[7], Description, ignore.case = TRUE), 1, 0),
    love = ifelse(grepl(common_phrases[9], Description, ignore.case = TRUE), 1, 0)
  )




#ordered logit model
print(colnames(df))

df$R.Rating<-as.numeric(df$R.Rating)
df<-df%>%mutate(Genre_GameShow=`Genre_Game Show`)
df<-df%>%mutate(Genre_Fiction=`Genre_-Fiction`)
df<-df%>%mutate(Genre_ActionAdventure=`Genre_Action & Adventure`)
df<-df%>%mutate(Genre_HomeGarden=`Genre_Home & Garden`)
df<-df%>%mutate(Genre_StandupTalk=`Genre_Stand-up & Talk`)
df<-df%>%mutate(R_Rated='R_Rated')
df$ordinal_IMDBRating <- as.factor(round(df$IMDB.Rating))
data<-as.data.frame(df)
#binary <- df[, c("(...)_R Rated", "Genre_Fiction", "Genre_ActionAdventure", "Genre_Animation", "Genre_Children",
#                 "Genre_Crime", "Genre_Drama", "Genre_Fantasy", "Genre_Horror",
#                 "Genre_Mystery", "Genre_Reality", "Genre_Thriller", "Genre_Anime",
#                 "Genre_Biography", "Genre_Comedy", "Genre_Cult", "Genre_Documentary",
#                 "Genre_Family", "Genre_Food", "Genre_GameShow", "Genre_LGBTQ",
#                 "Genre_Romance", "Genre_Sport", "Genre_Travel", "Genre_Musical",
#                 "Genre_HomeGarden", "Genre_StandupTalk", "Genre_Pet", "Genre_Unknown",
#                 "Platform.Count", "true_crime", "world_war", "award_winning",
#                 "serial_killer", "emmy_award", "featuring", "love")]
#count_zeros_ones <- function(x) {
#  return(c(zeros = sum(x == 0), ones = sum(x == 1)))
#}
#df$R_Rated<-as.integer(df$R_Rated)

#counts <- apply(binary, 2, count_zeros_ones)
#print(counts)
ologit = polr(ordinal_IMDBRating~R.Rating+No.of.Seasons+
                Genre_ActionAdventure+(Genre_Animation*Genre_Children)+
                Genre_Crime+Genre_Drama+Genre_Reality+Genre_Anime+
                +Genre_Comedy+Genre_Documentary+
                Platform.Count+featuring+love,
              data=df, Hess = T, method="logistic")

summary(ologit)

ologit_res = polr(ordinal_IMDBRating~R.Rating+
                    Genre_ActionAdventure+Genre_Children+
                    Genre_Reality+Genre_Anime+
                    +Genre_Comedy+Genre_Documentary+
                    Platform.Count+featuring,
                  data=df, Hess = T, method="logistic")

coeftest(ologit_res)
#H0: jointly insignificant
anova(ologit, ologit_res)

linearHypothesis(ologit, c("No.of.Seasons=0", "Genre_Animation=0", "Genre_Crime=0",
                           "Genre_Drama=0", "love=0", "Genre_Animation:Genre_Children=0",
                           "Genre_Anime=0"))

ologit2 = polr(ordinal_IMDBRating~R.Rating+
                 Genre_ActionAdventure+Genre_Children+
                 Genre_Reality
               +Genre_Comedy+Genre_Documentary+
                 Platform.Count+featuring,
               data=df, Hess = T, method="logistic")

coeftest(ologit2)
lipsitz.test(ologit2)
brant(ologit2)

logitgof(df$ordinal_IMDBRating, fitted(ologit2), g = 10,ord = TRUE)
pulkrob.chisq(ologit2, c("R.Rating"))
plot(brant(ologit2))


brant(ologit_res)




probit<-polr(ordinal_IMDBRating~R.Rating+
               Genre_ActionAdventure+Genre_Children+
               Genre_Reality
             +Genre_Comedy+Genre_Documentary+
               Platform.Count+featuring,
             data=df, Hess=T, method="probit")
summary(probit)
coeftest(probit)


vif(probit)

lipsitz.test(probit)

logitgof(df$ordinal_IMDBRating, fitted(probit), g = 10,ord = TRUE)
pulkrob.chisq(probit, c("R.Rating"))

library(VGAM)
df$ordinal_IMDBRating<-ordered(df$ordinal_IMDBRating)
gen_ordered_logit <- vglm(ordinal_IMDBRating~R.Rating+
                            Genre_ActionAdventure+Genre_Children+
                            Genre_Reality
                          +Genre_Comedy+Genre_Documentary+
                            Platform.Count+featuring,
                          cumulative(parallel = TRUE), data = df, model = TRUE)
#install.packages("gofcat")
library(gofcat)
lipsitz(gen_ordered_logit)
hosmerlem(gen_ordered_logit)


#the best
cont_ratio<-vglm(ordinal_IMDBRating~R.Rating+
                   Genre_ActionAdventure+Genre_Children+
                   Genre_Reality
                 +Genre_Comedy+Genre_Documentary+
                   Platform.Count+featuring,cratio(parallel = TRUE), data = df, model=TRUE)
lipsitz(cont_ratio)
hosmerlem(cont_ratio)

summary(cont_ratio)


c_loglog<-polr(ordinal_IMDBRating~R.Rating+
                 Genre_ActionAdventure+Genre_Children+
                 Genre_Reality
               +Genre_Comedy+Genre_Documentary+
                 Platform.Count+featuring,method="cloglog", data = df)

null_model<-vglm(ordinal_IMDBRating~1, cratio(parallel = FALSE), data = df, model=TRUE)
lipsitz(null_model)
#hosmerlem(ologit10)
#brant(ologit10)
lipsitz(c_loglog)
hosmerlem(c_loglog)

cat("Log Likelihood gen_ordered_logit:", logLik(gen_ordered_logit), "\n")
cat("AIC cont_ratio:", AIC(gen_ordered_logit), "\n")
cat("BIC cont_ratio:", BIC(gen_ordered_logit), "\n")
cat("Log Likelihood cont_ratio:", logLik(cont_ratio), "\n")
cat("AIC cont_ratio:", AIC(cont_ratio), "\n")
cat("BIC cont_ratio:", BIC(cont_ratio), "\n")
cat("Log Likelihood c_loglog:", logLik(c_loglog), "\n")
cat("AIC c_loglog:", AIC(c_loglog), "\n")
cat("BIC c_loglog:", BIC(c_loglog), "\n")
cat("Log Likelihood null:", logLik(null_model), "\n")
cat("AIC null:", AIC(null_model), "\n")
cat("BIC null:", BIC(null_model), "\n")
cat("Log Likelihood probit:", logLik(probit), "\n")
cat("AIC probit:", AIC(probit), "\n")
cat("BIC probit:", BIC(probit), "\n")
residuals(cont_ratio)
plot(residuals(cont_ratio))

PseudoR2(gen_ordered_logit)
library("DescTools")
PseudoR2(cont_ratio)
PseudoR2(c_loglog)
install.packages("modelsummary")
library(modelsummary)
residuals <- residuals(cont_ratio)
residuals1<-residuals(probit)

# Perform Breusch-Pagan test
#bptest(residuals ~ df$R.Rating+
#         df$Genre_ActionAdventure+df$Genre_Children+
#         df$Genre_Reality
#       +df$Genre_Comedy+df$Genre_Documentary+
#         df$Platform.Count+df$featuring)
#bptest(cont_ratio)

# plot(residuals ~ df$Genre_ActionAdventure)

#Marginal effects with robust estimator 

margeff(cont_ratio,vcov = vcovHC(your_model))

#ML

df_selected <- df[, c(
  2, 4, 6,                             
  10:25, 27:31, 34, 35,               
  39, 40, 41, 42, 43, 44, 45, 46,  
  47:50,                           
  3                                
)]

#install.packages(c("xgboost", "randomForest", "caret", "Matrix"))
library(xgboost)
library(randomForest)
library(caret)
library(Matrix)

df_model <- df_selected

#  data split
X <- df_model[, colnames(df_model) != "IMDB.Rating"]
y <- df_model$IMDB.Rating

#Random Forests

set.seed(123)
train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

rf_model <- randomForest(X_train, y_train, ntree = 500, importance = TRUE)
rf_preds <- predict(rf_model, X_test)

# calculate R2 and MAPE

r2 <- function(actual, predicted) {
  1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
}

mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

print(c("R²:", round(r2(y_test, rf_preds), 3)))
print(c("MAPE:", round(mape(y_test, rf_preds), 2)))

# feature importance
rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(
  Feature = rownames(rf_importance),
  Importance = rf_importance[, "IncNodePurity"]
)

ggplot(rf_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Random Forest Feature Importance", x = "Feature", y = "Importance") +
  theme_minimal()

# XGBoost

colnames(X_train) <- make.names(colnames(X_train))
colnames(X_test)  <- make.names(colnames(X_test))

X_train_matrix <- sparse.model.matrix(~ . -1, data = X_train)
X_test_matrix  <- sparse.model.matrix(~ . -1, data = X_test)

xgb_model <- xgboost(
  data = X_train_matrix,
  label = y_train,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

xgb_preds <- predict(xgb_model, X_test_matrix)

print(c("R²:", round(r2(y_test, xgb_preds), 3)))
print(c("MAPE:", round(mape(y_test, xgb_preds), 2)))

# feature importance

xgb_importance <- xgb.importance(model = xgb_model)

xgb.plot.importance(xgb_importance, top_n = 20, 
                    rel_to_first = TRUE, 
                    xlab = "Importance",
                    main = "XGBoost Feature Importance")

