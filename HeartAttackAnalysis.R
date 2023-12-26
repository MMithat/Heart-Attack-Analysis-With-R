data <- read.csv("heart.csv")

# install.packages("pastecs")
# install.packages("psych")
library(pastecs)
library(psych)
library(ggplot2)
library(stats)
library(dplyr)

#Checking Na values
sum(data.frame(is.na(data)))

#Adding gender as "integer"
gender_factor <- factor(data$sex, levels = c(0, 1), labels = c("Female", "Male"))
data <- cbind(data,gender_factor)

#GENEL BAKIŞ
barplot(table(data$gender_factor), col = c("pink","navy"))
describe(data)
head(data)


#3.1.	 INFERENCES ABOUT MEAN (One sample hypothesis testing)

# mu=200 olmasının sebebi ortalama sağlıklı bir insanın kolestrol değeri 200 ve altında olmalıdır
# fakat elimizde ki datada 246 olarak bulunmuştur. 
t.test(data$chol, mu=200)

t.test(data$thalach, mu=80)

ggplot(data, aes(x=1:length(chol),y= chol))+
  geom_point()+
  labs(x="Patiensts",y="Serum Cholestoral in mg/dl")+
  ggtitle("Distribution of Serum Cholestoral")+
  #geom_smooth(method = "lm", color = "green")+
  theme_minimal()


#Yaş T test
t.test(data$age, mu=50)
ggplot(data, aes(age))+
  labs(x="Age",y ="Number of People")+
  geom_bar()

#3.2.	 COMPARISONS OF MEANS (Two-sample hypothesis testing)
# Resting blood presure and maximum haert rate achieved
ggplot(data, aes(x = thalach ,y = trestbps)) +
  geom_point()+
  labs(x="Maximum Heart Rate Achieved", y="Resting Blood Presure(mm/Hg)" )+
  geom_vline(xintercept = mean(data$thalach), color="blue" ,size=1, linetype=5)+
  geom_hline(yintercept = mean(data$trestbps), color="blue", size=1, linetype=5)
  #geom_smooth(method = "lm", color="tomato")

wilcox.test(data$thalach,data$trestbps)
t.test(data$thalach,data$trestbps)
mean(data$trestbps)
mean(data$thalach)

# 3.3.	 INFERENCES ABOUT PROPORTIONS (One sample hypothesis testing)
#Probablity of Heart Attack
prop.test(sum(data$target), length(data$target), p= 0.5, alternative = "two.sided")

# 3.4.	 COMPARISONS OF PROPORTIONS (Two-sample hypothesis testing)
#Comprasions of probability of Heart Attack and Exercise Induced Angina 
prop.test(c(sum(data$target),sum(data$exang)),
          c(length(data$target),length(data$exang)),
          alternative = "two.sided")

# 3.5.	 SIMPLE AND MULTIPLE LINEAR REGRESSION

#simple
ggplot(data, aes(x = thalach, y = trestbps)) +
  geom_point(color = "navy") +
  geom_smooth(method = "lm")+
  labs(x = "Maximum Heart Rate Achieved", y = "Cholestrol level m/Hg") +
  ggtitle("Relationship between Maximum Heart Rate Achieved and  Resting Blood Presure") +
  theme_minimal()

#multiple 
lm(data=data, age~thalach+trestbps)


#3.6.	ONE-WAY or TWO-WAY ANOVA AND MULTIPLE COMPARISONS


            # Bu durumda, farklı göğüs ağrısı türleri arasında ulaşılan maksimum kalp 
            # hızında önemli farklılıklar olup olmadığını araştırabilirsiniz. 
            # Bu analiz, göğüs ağrısı ile maksimum kalp atış hızı arasındaki ilişki 
            # hakkında fikir verebilir.

  #One-way ANOVA
#Maximum Heart Rate and Chest Pain
aov(thalach~cp,data = data)
summary(aov(thalach~cp,data = data))

  # Two-way ANOVA
#Maximum Heart Rate ~ Chest Pain and Gender
lm(thalach~trestbps*sex, data = data)
summary(lm(thalach~trestbps*sex, data = data))

###########

#X=Age Y=Number of People who got heart attack
ggplot(data, aes(y=target,x=age))+
  geom_bar(stat="identity",fill="steelblue")+
  labs(x="Age",y="Number of People")+
  ggtitle("Number of People More Likely to Have Heart Attack")+
  theme_minimal()

# Probability of Heart Attack by Age
probability_of_age <- data %>%
  group_by(age) %>%
  summarise(heart_attack_prob = mean(target))
ggplot(data = probability_of_age, aes(x = age, y = heart_attack_prob)) +
  geom_point(color = "navy") +
  geom_smooth(method = "lm", color="purple")+
  labs(x = "Age", y = "Probability of Heart Attack") +
  ggtitle("Relationship between Age and Probability of Heart Attack") +
  theme_minimal()



heart_attack <- data %>% filter(target == 1)
no_heart_attack <- data %>% filter(target == 0)
t.test(heart_attack$age, no_heart_attack$age)

ggplot(data, aes(x = target, y = age)) +
  geom_boxplot() +
  labs(x = "Heart Attack", y = "Age") +
  ggtitle("Relationship between Age and Heart Attack") +
  theme_minimal()

#Relationship between Maximum Heart Rate Achieved and Probability of Heart Attack
heartrate_target <- glm(target ~ thalach, data = data, family = binomial)
thalach_range <- data.frame(thalach = seq(min(data$thalach), 
                                          max(data$thalach), 
                                          length.out = 100))
thalach_range$target_prob <- predict(heartrate_target, newdata = thalach_range,
                                     type = "response")
probability_of_thalach <- data %>%
  group_by(thalach) %>%
  summarise(heart_attack_prob = mean(target))
ggplot(data = probability_of_thalach, aes(x = thalach, y = heart_attack_prob)) +
  geom_point(color = "navy") +
  #geom_smooth(method = "lm")+
  labs(x = "Maximum Heart Rate Achieved", y = "Probability of Heart Attack") +
  ggtitle("Relationship between Maximum Heart Rate Achieved and Probability of Heart Attack") +
  theme_minimal()

# Age and Resting Blood Presure plot with linear regression line
ggplot(data, aes(x = age  ,y = trestbps)) +
  geom_point()+
  labs(x="Age", y="Resting Blood Presure ")+
  geom_smooth(method = "lm", color="tomato")

# Age and Maximum Heart Rate Achieved
ggplot(data, aes(x = age  ,y = thalach)) +
  labs(x="Age", y="Maximum Heart Rate Achieved")+
  geom_point()+
  geom_smooth(method = "lm", color="tomato")

#Age and Chol
ggplot(data, aes(x = age  ,y = chol)) +
  geom_point()+
  geom_smooth(method = "lm", color="tomato")

#Age and Oldpeak
ggplot(data, aes(x = age  ,y = oldpeak)) +
  geom_bar(stat = "identity")

#Chest Pain Type and Heart Attack Probability
ggplot(data,aes(x=cp,y=target))+
  geom_bar(stat="identity")

# ggplot(data = data, aes(x = thalach, y = target)) +
#   geom_point() +
#   geom_line(data = thalach_range, aes(x = thalach, y = target_prob), color = "blue") +
#   labs(x = "Maximum Heart Rate Achieved", y = "Probability of Heart Attack") +
#   ggtitle("Relationship between Maximum Heart Rate and Probability of Heart Attack") +
#   theme_minimal()

kalp_krizi_geçiren_erkek<- data$sex==1 & data $target ==1
kalp_krizi_geçiren_kadın <- data$sex==0 & data $target ==1
geçirmeyen_erkek <- data$sex == 1 & data$target == 0
geçirmeyen_kadın <- data$sex == 0 & data$target == 0

  
sum(kalp_krizi_geçiren_erkek)
sum(kalp_krizi_geçiren_kadın)
sum(geçirmeyen_erkek)
sum(geçirmeyen_kadın)



data_bygender_heartattack <- data.frame(
  Category = c("Man Having Heart Attack", "Woman Having Heart Attack", "Man Without Heart Attack", "Woman Without Heart Attack"),
  Count = c(sum(kalp_krizi_geçiren_erkek), sum(kalp_krizi_geçiren_kadın), sum(geçirmeyen_erkek), sum(geçirmeyen_kadın))
)

#heartatack_colors <- c("tomato","darkgreen","tomato","green")

ggplot(data_bygender_heartattack, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  #scale_fill_manual(values = heartatack_colors)+
  xlab("Category") +
  ylab("Count") +
  ggtitle("Having Heart Attack By Gender") +
  theme_minimal()

#gender and heart attack

chisq.test(table(data$gender_factor, data$target))


contingency_table <- table(data$gender_factor, data$target)
a <- contingency_table[,2]
b <- rowSums(contingency_table)
prop.test(a, b, alternative = "greater")


erkek_ratio <- sum(kalp_krizi_geçiren_erkek)/sum(geçirmeyen_erkek)
kadın_ratio <- sum(kalp_krizi_geçiren_kadın)/sum(geçirmeyen_kadın)

kadın_ratio/erkek_ratio
