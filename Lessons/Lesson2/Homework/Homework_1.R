# Find out, which __year__ was the __most terrific__ for portfolio you have identified as __most profitable__ during the lesson and 
# show it on the chart using `ggplot2` package. Write an explanation about your findings into the code as comment.
# __Commit__ it to your repository into `Lessons/Lesson2/Homework`.

## Code
install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

data_KPI <- read.csv("C:\\GeneralInsurance_Class-master\\Data\\lesson2_KPI.csv")

# zaporne premium sa nahradia nulou
data_KPI %>% mutate(Premium = ifelse(Premium < 0, 0, Premium))

#zoradenie data podla unit, kde pre kazdu unit mame vypocitany profit a nasledne zoradene od najvacsieho profitu
data_KPI %>% group_by(Unit) %>% mutate(profit=Premium-Expenses-Losses) %>% summarize(profit = sum(profit, na.rm = TRUE)) %>% arrange(desc(profit))

#zistili sme, ze najviac profitovy su v Unit7, preto dalej pracujeme uz len s Unit7 :
data_KPI %>% filter(Unit == "Unit7")  %>% mutate(profit=Premium-Expenses-Losses) %>% group_by(Year) %>% summarize(profit=sum(profit, na.rm = TRUE)) %>% arrange(profit)

#vykreslenie profitov unit7 za jednotlive roky
data_KPI %>% filter(Unit == "Unit7")  %>% mutate(profit=Premium-Expenses-Losses) %>% group_by(Year) %>% summarize(profit=sum(profit, na.rm = TRUE)) %>% arrange(profit) %>% ggplot(aes(x = Year, y = profit)) + geom_col()


# Your Explanation about analysis:
# nainstalovali sme a nacitali balicky dplyr a ggplot
# nacitali sme data
# pre kazdu unit sa vyratal profit a nasledne sa to zoradilo od najvacsieho profitu
# zistilo sa, ze najviac profitovy je Unit7, preto sa dalej budeme zaoberat len nim
# pre data z Unit7 sme spravili profit pre kazdy rok a zoradili od najmensieho profitu
# najmenej profitovym bol rok 2014 v Unit7
# najviac profitovym bol rok 2013 v Unit7
# nasledne sme to aj graficky znazornili ,kde tiez vidiet ktory rok bol ako velmi profitovy
