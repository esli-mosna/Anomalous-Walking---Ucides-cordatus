###Andada Bertioga 2018###
###Autor: Esli Mosna###

setwd("C:/Users/losma/OneDrive/Área de Trabalho/CRUSTA/Andada/dados_nova_versão")

#Ativar pacotes necessários
install.packages("rstatix") 
install.packages("multcompView")
library(FSA)
library(tidyverse)
library(psych)
library(cowplot)
library(readxl) #esse pacote é essencial, pois é com ele que vamos puxar os dados diretamente do excel
library(emmeans)
library(dbplyr)
library(car)
library(vegan)
library(rstatix)
library(multcompView)

histograma <- read_excel("andada_bertioga_2018.xlsx", 
                         sheet = "comparativo", range="h1:k22")

hist10 <- ggplot(histograma, aes(x=matriz_r, y=n_total, fill=type, width=10)) +
  geom_bar(stat="identity", color="#2d2e23", alpha = 0.8, position=position_dodge(width=10)) + #comando para colocar as barras lado a lado
  scale_fill_manual("type", values = c("#FFFFBF", "#1B7837"),limits=c("praia", "manguezal")) +
  labs(y="N", x="LC (mm)") +
  theme_bw() + 
  theme(legend.position = "none",
        legend.title =  element_text(),       
        axis.title.y=element_text(family="Arial", colour="#000000", face="bold", size=14, margin=margin(t=10,r=0,b=0,l=0)),
        axis.text.y=element_text(family="Arial", color="#000000", size=12, margin=margin(t=0,r=0,b=0,l=3)),
        axis.title.x=element_text(family="Arial", colour="#000000", face="bold", size=14),
        axis.text.x=element_text(family="Arial", color="#000000", size=12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  scale_y_continuous(limits=c(0,150), breaks=seq(0,150,50), expand=c(0,0)) +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10), expand=c(0,0))

hist10

ggsave("histograma.jpeg", plot = last_plot(), device = "jpeg", path = NULL, 
       scale = 1, width = 5, height = 4, units = c("in"),
       dpi = 600)

# polígono de frequência
polig <- ggplot(histograma, aes(x=matriz_r, y=n_total, color=type, group=type)) +
  geom_line(size=2, aes(group=type), color="black", show.legend = TRUE) +  # Linha de contorno para as linhas
  geom_line(aes(color=type), size=1.2) +  # Linha original sobreposta  
  scale_color_manual("type", values = c("praia" = "#FFFFBF", "manguezal" = "#1B7837")) +
  scale_fill_manual("type", values = c("praia" = "#FFFFBF", "manguezal" = "#1B7837")) +
  geom_ribbon(aes(ymin = 0, ymax = n_total, fill = type), alpha = 0.5) +  # Preencher a área com transparência
  geom_point(size=3.5, shape=21, fill="white", stroke=1.5, color="black") + # Adicionar contorno aos pontos
  labs(y="N", x="LC (mm)") +
  theme(legend.position = "none",
        legend.title =  element_text(),       
        axis.title.y=element_text(family="Arial", colour="#000000", face="bold", size=14, margin=margin(t=10,r=0,b=0,l=0)),
        axis.text.y=element_text(family="Arial", color="#000000", size=12, margin=margin(t=0,r=0,b=0,l=3)),
        axis.title.x=element_text(family="Arial", colour="#000000", face="bold", size=14),
        axis.text.x=element_text(family="Arial", color="#000000", size=12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  scale_y_continuous(limits=c(0,150), breaks=seq(0,150,50), expand=c(0,0)) +
  scale_x_continuous(limits=c(0,120), breaks=seq(0,120,10), expand=c(0,0))

polig

ggsave("polygon_freq_tcc.jpeg", plot = last_plot(), device = "jpeg", path = NULL, 
       scale = 1, width = 5, height = 4, units = c("in"),
       dpi = 600)

###Estágio Gonadal - gráfico###

estagios <- read_excel("andada_bertioga_2018.xlsx", 
              sheet = "estagio_gonadal", range="i1:m13")

estagios$estagio <- factor(estagios$estagio, levels = c("Maturo", "Em Maturação", "Imaturo"))

est_gon <- ggplot(estagios, aes(x = interaction(local, sexo), y = proporção, fill = estagio)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.3) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 2.5, fontface = "bold") +
  scale_fill_manual(values = c("Imaturo" = "#FFEE33", "Em Maturação" = "#E6550D",
                               "Maturo" = "#C00000")) +
  labs(y = "Proportion of crabs %", x = "Localidade e Sexo", fill = "Estágio") +
  theme_bw() +
  theme(axis.title.x=element_text(family="Arial", colour="#000000", face="bold", size=14),
        axis.title.y=element_text(family="Arial", colour="#000000", face="bold", size=14, margin=margin(t=10,r=0,b=0,l=0)),
        legend.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 12, color = "black", face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm")) +
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,10), expand=c(0,0))

est_gon

ggsave("stages_bar_proportion.jpeg", plot = last_plot(), device = "jpeg", path = NULL, 
       scale = 1, width = 5, height = 4, units = c("in"),
       dpi = 600)

###comparação_densidade###

densidade <- read_excel("andada_bertioga_2018.xlsx", 
                        sheet = "comparativo", range="d1:f30")

#SEPARANDO OS LOCAIS#
manguezal <- densidade[densidade$local == "manguezal", "densidade"]

praia <- densidade[densidade$local == "praia", "densidade"]

describeBy(manguezal$densidade)

describeBy(praia$densidade)

#Teste de Shapiro-Wilk para verificar a normalidade dos dados

shapiro.test(densidade$densidade)

shapiro.test(densidade$densidade[densidade$local=="manguezal"])

shapiro.test(densidade$densidade[densidade$local=="praia"])

#Teste de Levene para verificar a homocedasticidade dos dados#

#cálculo das variâncias
var1 <- var(densidade$densidade[densidade$local=="manguezal"])
var1
var2 <- var(densidade$densidade[densidade$local=="praia"])
var2

leveneTest(densidade~local, densidade)

#Comparação das médias (Teste T para dados paramétricos e Wicox para não paramétricos)#

t.test(densidade~local, densidade)

#plotagem comparação da densidade
densidade_ggplot <- ggplot(densidade, aes(x = local, y = densidade, fill = local)) +
  geom_boxplot(color = "black", size = 0.5, outlier.shape = NA) +
  stat_summary(fun.y = mean, geom = "point",  size = 4, color = "black") +
  labs (x = "Local", y = "Densidade (ind./m²)") +
  scale_x_discrete(labels = c("manguezal" = "Manguezal", "praia" = "Praia")) +
  scale_fill_manual(values = c("manguezal" = "#1B7837", "praia" = "#FFFFBF")) +
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))+# Ajuste a cor e o tamanho do contorno
  scale_y_continuous(limits=c(0,2), breaks=seq(0,2,0.5), 
                     minor_breaks = seq(0, 2, 0.25),expand = c(0, 0))

#boxplot violino mostrando valor médio
d <- ggplot(densidade, aes(x = local, y = densidade))

d + geom_violin(aes(fill = local), trim = FALSE) +
stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
             geom = "pointrange", color = "black") +
  scale_x_discrete(labels = c("manguezal" = "Mangrove", "praia" = "Beach")) +
  scale_fill_manual(values = c("manguezal" = "#1B7837", "praia" = "#FFFFBF")) +
  theme(axis.text = element_text(size = 12, color = "black", face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs (x = "Local", y = "Density (ind./m²)")


ggsave("density_violino_comparativo.jpeg", plot = last_plot(), device = "jpeg", path = NULL, 
       scale = 1, width = 5, height = 4, units = c("in"),
       dpi = 600)

###Comparação lc###

lc <- read_excel("andada_bertioga_2018.xlsx", 
                        sheet = "comparativo", range="a1:b631")

#SEPARANDO OS LOCAIS#
manguezal_lc <- lc[lc$local == "manguezal", "lc"]

praia_lc <- lc[lc$local == "praia", "lc"]

describeBy(manguezal_lc$lc)

describeBy(praia_lc$lc)

#Teste de Shapiro-Wilk para verificar a normalidade dos dados

shapiro.test(lc$lc)

shapiro.test(lc$lc[lc$local=="manguezal"])

shapiro.test(lc$lc[lc$local=="praia"])

#Teste de Levene para verificar a homocedasticidade dos dados#

#cálculo das variâncias
var3 <- var(lc$lc[lc$local=="manguezal"])
var3
var4 <- var(lc$lc[lc$local=="praia"])
var4

leveneTest(lc~local, lc)

#Comparação das médias (Teste T para dados paramétricos e Wicox para não paramétricos)#
wilcox.test(lc~local, lc)

#plotagem boxplot comparação de lc
lc_ggplot <- ggplot(lc, aes(x = local, y = lc, fill = local)) +
  geom_boxplot(color = "black", size = 0.5, outlier.shape = NA) +
  labs (x = "Local", y = "LC (mm)") +
  scale_x_discrete(labels = c("manguezal" = "Manguezal", "praia" = "Praia")) +
  scale_fill_manual(values = c("manguezal" = "#1B7837", "praia" = "#FFFFBF")) +
  theme(axis.text = element_text(size = 10, color = "black", face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))# Ajuste a cor e o tamanho do contorno

#boxplot violino com a mediana
l <- ggplot(lc, aes(x = local, y = lc))

l + geom_violin(aes(fill = local), trim = FALSE) +
  geom_boxplot(width = 0.15) +
  scale_x_discrete(labels = c("manguezal" = "Mangrove", "praia" = "Beach")) +
  scale_fill_manual(values = c("manguezal" = "#1B7837", "praia" = "#FFFFBF")) +
  theme(axis.text = element_text(size = 12, color = "black", face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs (x = "Local", y = "CW (mm)")

ggsave("CW_violino.jpeg", plot = last_plot(), device = "jpeg", path = NULL, 
       scale = 1, width = 5, height = 4, units = c("in"),
       dpi = 600)

#lc entre as subáreas do manguezal

lc_mangue<- read_excel("andada_bertioga_2018.xlsx", 
           sheet = "lc_mangue", range="a1:b376")

shapiro.test(lc_mangue$lc) ###teste de Shapiro-Wilk###

leveneTest(lc~local, lc_mangue) ###teste de levene###

wilcox.test(lc~local, lc_mangue) ###teste de wilcoxon comparando as medianas###

#boxplot violino com a mediana
box_lc__mangue <- ggplot(lc_mangue, aes(x = local, y = lc))

box_lc__mangue + geom_violin(aes(fill = local), trim = FALSE) +
  geom_boxplot(width = 0.15) +
  scale_x_discrete(labels = c("Interna" = "Internal", "Franja" = "Fringe")) +
  scale_fill_manual(values = c("Interna" = "#1B7837", "Franja" = "#FFFFBF")) +
  theme(axis.text = element_text(size = 12, color = "black", face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs (x = "Subarea", y = "CW (mm)")

ggsave("CW_subarea.jpeg", plot = last_plot(), device = "jpeg", path = NULL, 
       scale = 1, width = 5, height = 4, units = c("in"),
       dpi = 600)

#densidade entre as subáreas do manguezal

densidade_mangue<- read_excel("andada_bertioga_2018.xlsx", 
                       sheet = "densidade_manguezal", range="v1:x21")

shapiro.test(densidade_mangue$densidade) ###teste de Shapiro-Wilk###

leveneTest(densidade~subarea, densidade_mangue) ###teste de levene###

t.test(densidade~subarea, densidade_mangue)

box_d_mangue <- ggplot(densidade_mangue, aes(x = subarea, y = densidade))

box_d_mangue + geom_violin(aes(fill = subarea), trim = FALSE) +
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black") +
  scale_x_discrete(labels = c("interna" = "Internal", "franja" = "Fringe")) +
  scale_fill_manual(values = c("interna" = "#1B7837", "franja" = "#FFFFBF")) +
  theme(axis.text = element_text(size = 12, color = "black", face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs (x = "Subarea", y = "Density (ind./m²)")

ggsave("CW_subarea.jpeg", plot = last_plot(), device = "jpeg", path = NULL, 
       scale = 1, width = 5, height = 4, units = c("in"),
       dpi = 600)

#densidade entre as subáreas da praia

densidade_praia<- read_excel("andada_bertioga_2018.xlsx", 
                       sheet = "comparativo", range="e1:f10")

shapiro.test(densidade_praia$densidade) ###teste de Shapiro-Wilk###

leveneTest(densidade~subarea, densidade_praia) ###teste de levene###

t.test(densidade~subarea, densidade_praia)

#lc entre as áreas da praia

lc_praia<- read_excel("andada_bertioga_2018.xlsx", 
                             sheet = "densidade_praia", range="o1:p256")

shapiro.test(lc_praia$lc) ###teste de Shapiro-Wilk###

leveneTest(lc~subarea, lc_praia) ###teste de levene###

wilcox.test(lc~subarea, lc_praia)

#anova pluviosidade
chuva<- read_excel("pluviosidade_bertioga.xlsx", 
                      sheet = "dias_eai", range="n1:o91")

anova_1 <- aov(ano~pluviosidade, data=chuva)

summary(anova_1)                         

                      
                         