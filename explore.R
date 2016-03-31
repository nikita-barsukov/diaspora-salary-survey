library(ggplot2)
library(scales)
library(ggthemr)
df = read.csv('raw_data/Sheet_1.csv')
df = df[,10:13]
colnames(df) = c('salary.gross', 'salary.net','in.cph','gender')
df$tax.rate = 1-df$salary.net/df$salary.gross
#df = df[df$tax.rate > 0,]

ggthemr('fresh')

pl = ggplot(data=df, aes(x=salary.gross, y=salary.net)) + 
  geom_point(aes(color=gender)) +
  stat_smooth(method = "lm", se = FALSE) +
  ggtitle('Зарплата диаспорян до и после налогов') +
  scale_color_discrete(labels=c("Женщины", "Мужчины"), name='') +
  scale_x_continuous(name='Грязная зарплата', labels=comma) +
  scale_y_continuous(name='Чистая зарплата', labels=comma)
print(pl)

print(paste('Mean salary overall:', mean(df$salary.gross)))
print(aggregate(salary.gross ~ gender, df, mean))
print(aggregate(salary.gross ~ in.cph, df, mean))

##  gross vs net salary

lm.obj = lm(salary.net ~ salary.gross, data=df)
print(lm.obj)
smr = summary(lm.obj)
print(paste('R-sq for model', smr$adj.r.squared))

# t.test
print('T-test on gross salary for men and women')
t.test.obj = t.test(salary.gross ~ gender, data=df, alternative='less')
print(t.test.obj)

# STATS INFO
type = c("Всего", "Женщины", "Мужчины", "Копенгаген")
dk.salary = c(38957.98,35914.89,41801.62,42724.35)
diaspora.salary = c(mean(df$salary.gross), 
                    aggregate(salary.gross ~ gender, df, mean)[,2],
                    aggregate(salary.gross ~ in.cph, df, mean)[2,2])
compare.df = cbind.data.frame(type, dk.salary, diaspora.salary)

library(reshape)
compare.df.melt = melt(compare.df)

pl.hist = ggplot(data=hist.df) + 
  geom_histogram(aes(x=value),
                 binwidth = 10000, color='white') + 
  facet_grid(variable ~ .) +
  ggtitle('Распределение зарплат диаспорян') +
  scale_x_continuous(name='Месячная зарплата', labels=comma) +
  scale_y_continuous(name='Число анкет') 
print(pl.hist)

pl.means = ggplot(data = compare.df.melt,aes(x=type, y=value)) + 
  geom_bar(data=subset(compare.df.melt, variable=='dk.salary'), 
           stat='identity', width=.05, fill='#ababab') +
  geom_point(size=5, aes(color=variable)) +
  scale_y_continuous(labels=comma, name='') +
  scale_x_discrete(name='') +
  scale_color_discrete(labels=c("Дания","Диаспора"), name="") +
  ggtitle(expression(atop("Средняя зарплата по группам", 
                          atop(italic("Данные зарплатного опроса и Danmarks Statistik"), "")))) +
  coord_flip()

print(pl.means)
