#Relatorio de Curso Introdução ao R

# Carregar os pacotes
library(tidyverse) 

#Carregar base de dados:
us_births <- read.table(file = "US_births_2000-2014_SSA.csv",
                     header = TRUE,
                     sep = ",",)

#Conferir o nome das colunas
names(us_births)

# Visualizar a estrutura desse banco de dados
glimpse(us_births)

#transformando as colunas "year", "month", "date_of_month" e "day_of_week" 
#em tipo FACTOR:
us_births <- us_births %>%
              mutate(year = as.factor(year),
                     month = as.factor(month),
                     date_of_month = as.factor(date_of_month),
                     day_of_week = as.factor(day_of_week))

glimpse(us_births)

#Conferindo as dimensões da tabela:
dim(us_births)  
nrow(us_births) 
ncol(us_births)

#transformando as colunas em objetos:
attach(us_births)

# Obtendo as medias e desvio padrão de nascimentos por mês:
tapply(births, month, mean)
tapply(births, month, sd)

# Obtendo as medias e desvio padrão de nascimentos por dia da semana:
tapply(births, day_of_week, mean)
tapply(births, day_of_week, sd)

# Obtendo as medias e desvio padrão de nascimentos por mês:
tapply(births, year, mean)
tapply(births, year, sd)



us_births_groupby <- us_births %>% 
                          group_by(year) %>% 
                          summarise(total_year = sum(births))


us_births_groupby

# Criando a area de plotagem
# Funcao 'ggplot()'
ggplot(us_births_groupby)

ggplot(us_births_groupby, aes(x=year, y=total_year, group=1)) +
  geom_line()+
  geom_point()+
  labs(x = "Year", 
       y = "Number of Births",
       title = "Births by year (2000-2014") 



#nascidos na sexta-feira:
friday <- us_births %>% 
  filter(day_of_week == '5') 

mean(friday$births)

#nascidos em dia 6(sexta_feira) e data 13:
friday13 <- us_births %>% 
  filter(day_of_week == '5', 
         date_of_month == '13')

mean(friday13$births)

tapply(friday13$births, friday13$month, mean)
tapply(friday13$births, friday13$month, sd)
