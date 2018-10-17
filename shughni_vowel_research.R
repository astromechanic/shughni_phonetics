#Данный код считает статистику для акустического исследования шугнанских гласных.

library(tidyverse)
library(Hmisc)
library(lme4)
library(effects)
library(optimx)

#Первая часть кода — описание статистики для гласных a-a:, i-i:, u-u:
#Рабочий датасет - df_1
df_1 <- read.csv('/Users/violaivanova/Documents/shughni/report2018/datasets/csv/LT_MF_SM_1_3.csv', head=TRUE, sep=',')
# унификация того, что записано в колонке trial и удаление лишних строк (произнесения 5, 6, 7 которые есть
# только у одного информанта и только для одного слова)
df_1$trial <- gsub("c",4,df_1$trial)
df_1$trial <- gsub("с",4,df_1$trial)
df_1$trial <- as.factor(df_1$trial)
df_1 <- df_1[-c(75, 76, 77, 78, 79), ]

#В обработке данных не будут учитываться произнесения в предложении.
df_1 <- subset(df_1, df_1$trial == "1" | df_1$trial =="2" | df_1$trial == 3)
df_1$trial <- factor(df_1$trial)
levels(df_1$trial)

#Нормирование F1 и F2 и длительность по носителю
df_1$zF1 <- ave(df_1$F1, df_1$subject, FUN=scale)
df_1$zF2 <- ave(df_1$F2, df_1$subject, FUN=scale)
df_1$zDuration <- ave(df_1$duration, df_1$subject, FUN=scale)

#Добавляем информацию о гласных и носителях.
df_1 <- mutate(df_1, vowel_length = as.factor(ifelse(grepl(":",sound), "long", "short")))
df_1 <- mutate(df_1, vowel_sound = as.factor(ifelse(
  grepl("i",sound), "i", 
  ifelse(grepl("a",sound), "a", "u"))))
df_1 <- mutate(df_1, raise = as.factor(ifelse(grepl("a",sound), "low", "high")))
df_1 <- mutate(df_1, backness = as.factor(ifelse(grepl("i",sound), "front", "back")))

df$SpkrSound <- paste(df$subject, " ")
df$SpkrSound <- paste(df$SpkrSound, df$sound)

#Посмотрим на данные, которые мы загрузили
#Для всех носителей:
df_1 %>% 
  ggplot(aes(F2, F1, label=sound, color = short_long, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  facet_grid(trial~subject)+
  theme_bw()+
  stat_ellipse()

#Для отдельного носителя (нужно изменить сабсет по LT, MF или SM)
df_sub <- subset(df_1, df_1$subject == "SM")
df_sub %>% 
  ggplot(aes(F2, F1, label=sound, color = short_long, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  facet_wrap(~trial)+
  theme_bw()+
  stat_ellipse()+
  ggtitle("Speaker SM")
 
## Статистические модели для гласных a, i, u
# Сначала разберемся с различием по качеству
summary(model.1 <- glmer(short_long ~ zF1:vowel_type+zF2:vowel_type+zF1:zF2:vowel_type+(1|subject/trial), 
family = binomial, data = df_1, 
control = glmerControl(optimizer ='bobyqa')))
drop1(model.1, test="Chisq")

#А теперь обратимся к зависимости словарной долготы от физической длительности
summary(model.2 <- glmer(short_long ~ zDuration:vowel_type + 
                           (1|subject/trial), 
                         family = binomial, data = df_1, 
                         control = glmerControl(optimizer ='bobyqa')))
drop1(model.2, test="Chisq")

#Визуализация значимости длительности для противопоставления по долготе

df_sub_1 <- subset(df_1, subject == "SM")
df_sub_1 %>% 
  ggplot(aes(zDuration, as.numeric(short_long)-1, label = vowel_type))+
  geom_text()+
  facet_grid(vowel_type~subject)+
  geom_smooth(method = "glm",method.args = list(family = "binomial"))+
  xlab("zDuration") +
  ylab("The probability of getting a SHORT vowel")



###----------------------- вторая часть -----------------------###
df_2 <- read.csv('/Users/violaivanova/Documents/shughni/report2018/datasets/csv/lat_mar_sha_2.csv', head=TRUE, sep=';')

# унификация того, что записано в колонке trial и удаление лишних строк (произнесения 5, 6, 7 которые есть
# только у одного информанта и только для одного слова)
df_2$trial <- gsub("c",4,df_2$trial)
df_2$trial <- gsub("с",4,df_2$trial)
df_2$trial <- as.factor(df_2$trial)
df_2 <- df_2[-c(213, 214, 215, 284), ]

# нормирование F1 и F2 и длительности по субъекту (носителю)
df_2$zF1 <- ave(df_2$F1, df_2$subject, FUN=scale)
df_2$zF2 <- ave(df_2$F2, df_2$subject, FUN=scale)
df_2$zDuration <- ave(df_2$duration, df_2$subject, FUN=scale)

# добавляем информацию о гласных (backness and raise)
df_2 <- mutate(df_2, vowel_length = as.factor(ifelse(grepl("long",short.long), "long", "short")))
df_2 <- mutate(df_2, vowel_sound = as.factor(ifelse(
  grepl("i",sound), "i", 
  ifelse(grepl("e",sound), "e", "ɛ"))))

df_2$SpkrSound <- paste(df_2$subject, " ")
df_2$SpkrSound <- paste(df_2$SpkrSound, df_2$sound)

df_2 <- mutate(df_2, vowel_sound = as.factor(sound))

#Убираем произнесения в предложении
df_2 <- subset(df_2, df_2$trial == "1" |df_2$trial == "2" |df_2$trial == "3")
df_2$trial <- factor(df_2$trial)
levels(df_2$trial)

#Посмотрим на полученные данные
#Для отдельного носителя (нужно изменить сабсет по LT, MF или SM)
df_sub <- subset(df_2, df_2$subject == "Shamse")
df_sub %>% 
  ggplot(aes(F2, F1, label=sound, color = sound, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  facet_wrap(~trial)+
  theme_bw()+
  stat_ellipse()+
  ggtitle("Speaker SM")

# e and e open
df_e <- subset(df_2, df_2$sound == "e")
df_eopen <- subset(df_2, df_2$sound == "ɛ")
df_e_eopen <- rbind(df_e, df_eopen)
df_e_eopen$vowel_type <- as.factor(df_e_eopen$vowel_type)
df_e_eopen$vowel_type <- factor(df_e_eopen$vowel_type)
levels(df_e_eopen$vowel_type)

df_e_eopen <- mutate(df_e_eopen, zSubject = as.factor(ifelse(
  grepl("Marifat",subject), "MF", 
  ifelse(grepl("Shamse",subject), "SM", "LT"))))

df_e_eopen %>% 
  ggplot(aes(F2, F1, label=sound, color = sound, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()+
  stat_ellipse()+
  facet_grid(~zSubject)


summary(model.2.1 <- glmer(vowel_type ~ zF1*zF2 + zDuration +
                           (1|subject/trial), 
                         family = binomial, data = df_e_eopen, control = glmerControl(
                           optimizer ='bobyqa')))
drop1(model.2.1, test = "Chisq")


summary(model.2.2 <- glmer(vowel_type ~ zF1 + zF2 + zDuration +
                           (1|subject/trial), 
                         family = binomial, data = df_e_eopen, control = glmerControl(
                           optimizer ='bobyqa')))
drop1(model.2.2, test = "Chisq")
anova(model.2.1, model.2.2)
#останавливаемся на модели 1

###### e and i
df_e <- subset(df_2, df_2$sound == "e")
df_i <- subset(df_2, df_2$sound == "i")
df_s <- rbind(df_e, df_i)
df_s$vowel_type <- as.factor(df_s$vowel_type)
df_s$vowel_type <- factor(df_s$vowel_type)
levels(df_s$vowel_type)

df_s <- mutate(df_s, zSubject = as.factor(ifelse(
  grepl("Marifat",subject), "MF", 
  ifelse(grepl("Shamse",subject), "SM", "LT"))))

df_s %>% 
  ggplot(aes(F2, F1, label=sound, color = sound, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()+
  stat_ellipse()+
  facet_grid(~zSubject)

summary(model.03 <- glmer(vowel_type ~ zF1*zF2 + zDuration +
                             (1|subject/trial), 
                           family = binomial, data = df_s, control = glmerControl(
                             optimizer ='bobyqa')))
drop1(model.03, test="Chisq")

summary(model.04 <- glmer(vowel_type ~ zF1 + zF2 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.04, test="Chisq")

# e open and i short
df_eopen <- subset(df_2, df_2$sound == "ɛ")
df_i <- subset(df_2, df_2$sound == "i")
df_s <- rbind(df_eopen, df_i)
df_s$vowel_type <- as.factor(df_s$vowel_type)
df_s$vowel_type <- factor(df_s$vowel_type)
levels(df_s$vowel_type)

df_s <- mutate(df_s, zSubject = as.factor(ifelse(
  grepl("Marifat",subject), "MF", 
  ifelse(grepl("Shamse",subject), "SM", "LT"))))

df_s %>% 
  ggplot(aes(F2, F1, label=sound, color = sound, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()+
  stat_ellipse()+
  facet_grid(~zSubject)

summary(model.05 <- glmer(vowel_type ~ zF1*zF2 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.05, test="Chisq")

summary(model.06 <- glmer(vowel_type ~ zF1 + zF2 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.06, test="Chisq")

summary(model.07 <- glmer(vowel_type ~ zF1 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.07, test="Chisq")

# ------- eː и iː

df_e <- subset(df_2, df_2$sound == "e")
df_ilong <- subset(df_2, df_2$sound == "i:")
df_s <- rbind(df_e, df_ilong)
df_s$vowel_type <- as.factor(df_s$vowel_type)
df_s$vowel_type <- factor(df_s$vowel_type)
levels(df_s$vowel_type)

df_s <- mutate(df_s, zSubject = as.factor(ifelse(
  grepl("Marifat",subject), "MF", 
  ifelse(grepl("Shamse",subject), "SM", "LT"))))

df_s %>% 
  ggplot(aes(F2, F1, label=sound, color = sound, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()+
  stat_ellipse()+
  facet_grid(~zSubject)

summary(model.10 <- glmer(vowel_type ~ zF1*zF2 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.10, test="Chisq")

summary(model.11 <- glmer(vowel_type ~ zF1*zF2 +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.11, test="Chisq")

###
### ɛː и iː
###
df_eopen <- subset(df_2, df_2$sound == "ɛ")
df_ilong <- subset(df_2, df_2$sound == "i:")
df_s <- rbind(df_eopen, df_ilong)
df_s$vowel_type <- as.factor(df_s$vowel_type)
df_s$vowel_type <- factor(df_s$vowel_type)
levels(df_s$vowel_type)

df_s <- mutate(df_s, zSubject = as.factor(ifelse(
  grepl("Marifat",subject), "MF", 
  ifelse(grepl("Shamse",subject), "SM", "LT"))))

df_s %>% 
  ggplot(aes(F2, F1, label=sound, color = sound, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()+
  stat_ellipse()+
  facet_grid(~zSubject)

#Такая модель не сходится
summary(model.20 <- glmer(vowel_type ~ zF1*zF2 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))

# и такая тоже не сходится
summary(model.21 <- glmer(vowel_type ~ zF1 + zF2 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
# get ready for perfect separation
summary(model.22 <- glm(vowel_type ~ zF1 + zF2,family = binomial, data = df_s ))

#Не окончательная модель, но первая, которая сошлась
Formula: sound ~ zF2 + zF1:zF2 + zDuration + (1 | subject)

summary(model.23 <- glmer(vowel_type ~ zF2 + zF1:zF2 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.23, test="Chisq")

summary(model.24 <- glmer(vowel_type ~ zF2 + zF1:zF2 +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))

summary(model.1.0 <- glmer(vowel_type ~ zF1 +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.1.0, test="Chisq")


### --- i и iː
df_i <- subset(df_2, df_2$sound == "i")
df_ilong <- subset(df_2, df_2$sound == "i:")
df_s <- rbind(df_i, df_ilong)
df_s$vowel_type <- as.factor(df_s$vowel_type)
df_s$vowel_type <- factor(df_s$vowel_type)
levels(df_s$vowel_type)

df_s <- mutate(df_s, zSubject = as.factor(ifelse(
  grepl("Marifat",subject), "MF", 
  ifelse(grepl("Shamse",subject), "SM", "LT"))))

df_s %>% 
  ggplot(aes(F2, F1, label=sound, color = sound, group = sound))+
  geom_text()+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()+
  stat_ellipse()+
  facet_grid(~zSubject)

levels(df_s$sound)
df_s$sound <- factor(df_s$sound)
summary(model.fin <- glmer(sound ~ zF1*zF2 + zDuration +
                            (1|subject/trial), 
                          family = binomial, data = df_s, control = glmerControl(
                            optimizer ='bobyqa')))
drop1(model.fin, test="Chisq")

summary(model.fin2 <- glmer(sound ~ zF1 + zF2 + zDuration +
                             (1|subject/trial), 
                           family = binomial, data = df_s, control = glmerControl(
                             optimizer ='bobyqa')))
drop1(model.fin2, test="Chisq")
