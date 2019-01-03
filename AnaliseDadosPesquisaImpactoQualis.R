library("ggplot2")
library("ggthemes")
library("scales")
library(readxl)
library(RColorBrewer)
library(magrittr)
library(plotly)
library(dplyr)
library(reshape2)
library(Hmisc)


#importando arquivo excel

revistas_total <- read_excel("Planilha_Qualis_NA.xlsx", col_types = c("text", "text", "text", 
                                                                      "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric"))

Qtotal <- read_excel("Qualis_Lista.xlsx", col_types = c("text", "text", "text"))
dataq <-data.frame((Qtotal))
dataq
dataq <- dataq %>% 
  group_by(Estrato) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>%
  arrange(desc(Estrato))
summary(dataq)
dataq$label <- scales::percent(dataq$per)
#Gráfico 1 - Distribuição estrato Qualis-Educação 
ggplot(data=dataq)+
  geom_bar(aes(x="", y=per, fill=Estrato), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=paste0(Estrato, " - ", label)), size=4) +
  ggsave("grafico1.jpg")

#Gráficos 1 média jif estrato qualis
dados1<-revistas_total[revistas_total$Estrato=="A1",]
dados2<-revistas_total[revistas_total$Estrato=="A2",]
dados3<-revistas_total[revistas_total$Estrato=="B1",]
dados4<-revistas_total[revistas_total$Estrato=="B2",]
dados5<-revistas_total[revistas_total$Estrato=="B3",]
dados6<-revistas_total[revistas_total$Estrato=="B4",]
dados7<-revistas_total[revistas_total$Estrato=="B5",]
MediaJif <- c(mean(na.omit(dados7$JIF)),  mean(na.omit(dados6$JIF)), mean(na.omit(dados5$JIF)), mean(na.omit(dados4$JIF)), mean(na.omit(dados3$JIF)), mean(na.omit(dados2$JIF)), mean(na.omit(dados1$JIF)))
MediaJif
mdj <- data.frame(MediaJif)
names(MediaJif) <- c( "B5", "B4", "B3", "B2", "B1", "A2", "A1")        
MediaJif
barplot(MediaJif, ylab = "JIF", xlab = "Estrato Qualis", col=theme_classic())  #+ ggsave("media_jif_qualis.png")

# gráfico 3 - Média do JIF por estrato Qualis 
ggplot(revistas_total,aes(group=Estrato,y=JIF,x=reorder(Estrato, desc(Estrato)), fill=Estrato)) +
  stat_summary(geom = "bar",fun.data = median_hilow) +
  stat_summary(fun.y=mean,geom="point") + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar") + 
  stat_summary(fun.y=mean,geom="line",aes(group=""),linetype="dashed")  + theme_light() + xlab("Qualis")+
  ylab("JIF") + ggsave("grafico3.jpg")

# gráfico 4 - Média do SJR por estrato Qualis  
ggplot(revistas_total,aes(group=Estrato,y=`Hindex SJR`,x=reorder(Estrato, desc(Estrato)), fill=Estrato)) +
  stat_summary(geom = "bar",fun.data = median_hilow) +
  stat_summary(fun.y=mean,geom="point") + 
  stat_summary(fun.data=mean_cl_normal, geom="errorbar") + 
  stat_summary(fun.y=mean,geom="line",aes(group=""),linetype="dashed")  + theme_light() + xlab("Qualis")+
  ylab("SJR") + ggsave("grafico4.jpg")
# gráfico 5 - Boxplot Qualis educação com JCR 
ggplot(revistas_total, aes(group=Estrato,y=JIF,x=reorder(Estrato, desc(Estrato)))) + 
  geom_boxplot() + 
  #  geom_jitter() +
  #scale_x_discrete(limits=rev(levels(Estrato))) +
  #  scale_y_log10() +
  labs(x="Estrato Qualis", y = "JIF") +
  theme_light() +
  ggsave("grafico5.jpg")
# gráfico 6 - Boxplot Qualis educação com SJR 

ggplot(revistas_total, aes(group=Estrato,y=`Hindex SJR`,x=reorder(Estrato, desc(Estrato)))) + 
  geom_boxplot() + 
  #  geom_jitter() +
  #scale_x_discrete(limits=rev(levels(Estrato))) +
  #  scale_y_log10() +
  labs(x="Estrato Qualis", y = "SJR") +
  theme_light() +
  ggsave("grafico6.jpg")

# gráfico 7 - Distribuição dos periódicos indexados no JCR no estrato Qualis-Educação 
jcrQualis <- read_excel("distribuicao_JCR_Qualis.xlsx", col_types = c("text", "numeric"))

jcrQualis <- data.frame(
  Estrato = c("NA", "A1", "A2", "B1", "B3", "B4"),
  value = c(83, 6, 9, 1, 0.5, 0.5)
)

jcrQualis
library(ggpubr)
labs <- paste0(jcrQualis$Estrato, " (", jcrQualis$value, "%)")
ggpie(jcrQualis, "value", label = "Estrato", lab.pos = "in",lab.font =c(4,"black"),
      fill = "Estrato", ggtheme=theme_light(), lab.adjust=1)


ggplot(jcrQualis, aes(x= factor(1), y=value, fill=Estrato))+
  geom_bar(width = 2, stat = "identity") + coord_polar("y", start=0) + theme_void()+
  #geom_text(aes(x=factor(5), y = cumsum(value) - value/2, label=paste0(Estrato, " - ",  percent(value/100))), size=4) +
  ggsave("grafico7.jpg")

# gráfico 8 - Distribuição dos periódicos Qualis com JCR, por estrato
jcrQualisEst <- read_excel("distribuicao_JCR_Qualis_estrato.xlsx", col_types = c("text", "numeric"))
ggplot(jcrQualisEst, aes(x= factor(1), y=Valores, fill=Estrato))+
  geom_bar(width = 2, stat = "identity") + coord_polar("y", start=0) + theme_void()+
  ggsave("grafico8.jpg")

# Gráfico 9 - Article Influence X JIF em relação ao Qualis-Educação
revistas_totalSJ <- read_excel("Planilha_Qualis_NA_Sem_Jama2.xlsx", col_types = c("text", "text", "text", 
                                                                                  "numeric", "numeric", "numeric", "numeric", 
                                                                                  "numeric", "numeric"))
ggplot(revistas_totalSJ, aes(x=JIF, y = `Article influence`, 
                             col=Estrato)) +
  geom_point(size=4) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.position = "top") +
  ggsave("grafico9.jpg")
