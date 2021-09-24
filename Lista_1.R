# Entregar até o dia 07/07 - Grupos de até seis pessoas 
# Incluir os comentários e interpretações e compilar (knit) para o formato .docx
# Em todos os itens, descreva a finalidade de cada função e os argumentos utilizados

# 1- a)  Crie uma função chamada varios, com finalidade de calcular as seguintes 
#informações: mínimo, máximo, mediana, média , variância e desvio padrão

# b) Aplique  a função varios no vector x:
library (emmeans)
library (MASS)
library (ggplot2)
library (myfuns)
library (sjstats)
library (R6)
library (nortest)
library (cld2)
library (multcomp)
library (multcompView)
library(purrr)
library(myfuns)



set.seed(123)
x<-rnorm(n=60, mean=28, s=7)


mean(x) #M�dia

max(x) #M�ximo
min(x) #M�nimo

var(x)  #Varian�a

median(x) #Mediana

sd(x)   #Desvio padr�o

range(x) #Amplitude Max e Min


# c) Grafique o box-plot do vetor  z e interprete os resultados

z<-c(x, c(3,4,50,70)) #c=concatenar

hist(z) #Histograma

frequency.polygon(z) #Gr�fico

boxplot(z)

# 2 - A área foliar (cm2) de mudas de Spondias purpurea em função da classe do solo foi avaliada em um experimento realizado no DIC, com  $`r paste(k)`$ repetições.
#Os dados obtidos foram os seguintes:

#Argissolos=c(41.34322525,38.38308972, 38.97390626,31.63958896)

#Cambissolos=c(36.66798454,22.78348048, 16.76976829,27.3451723)

#Latossolos=c(37.81177863,35.41822361,24.12985781, 18.46962888)

#Neossolos=c(15.44074588,28.70434713,17.29227191, 11.27180798)

#dentro dos trat (solos) usar 1 e 2 vs 3 e 4
dados<-data.frame(
              solos = c(1L,1L,1L,1L,1L,2L,2L,
                        2L,2L,2L,3L,3L,3L,3L,3L,4L,4L,4L,4L,4L,5L,
                        5L,5L,5L,5L,6L,6L,6L,6L,6L),
             blocos = c(1L,2L,3L,4L,5L,1L,2L,
                        3L,4L,5L,1L,2L,3L,4L,5L,1L,2L,3L,4L,5L,1L,
                        2L,3L,4L,5L,1L,2L,3L,4L,5L),
                 AF = c(41.34322525,38.38308972,
                        38.97390626,31.63958896,36.66798454,22.78348048,
                        16.76976829,27.3451723,37.81177863,35.41822361,24.12985781,
                        18.46962888,15.44074588,28.70434713,17.29227191,
                        11.27180798,19.0075966,14.63392875,24.06689838,
                        4.014525839,15.29583545,11.09286136,6.412227478,11.94540392,
                        15.44452119,50.21190468,48.90082796,54.53059803,
                        39.81937162,47.23100813)
       )

levels(dados$solos) <- c("Argissolos", "Cambissolos", "Latossolos", "Neossolos","Vertissolos", "Planossolos")

dados2$H<-as.factor(dados2$H)
dados2$blocos<-as.factor(dados2$blocos)


dados$solos<-as.factor(dados$solos)

#Hip�teses
#H0=mAs m�dias s�o iguais
#Ha= Ao menos uma m�dia � diferente

#a) Obtenha o quadro da Anova, incluindo o CV.

modelo<-lm(AF~blocos+solos, data=dados)

modelo

summary(modelo)

anovaCV(modelo)

#b) Aplique o teste de Tukey.

medias<-emmeans(modelo, ~solos)

medias

letras<- multcomp::cld(medias, adjust="tukey", Letters=letters, reversed=T)

letras

#c) Apresente os resultados de em uma figura, utilizando ggplot2. 

plot(letras, horizontal = F)

mediasPT<-as.data.frame(letras)

mediasPT

class(mediasPT)


ggplot(data=mediasPT, aes(y=emmean, x=solos))+
  geom_bar(stat="identity", fill="red")+
  geom_text(aes(label=.group), y=0.1*max(mediasPT$emmean))+
  xlab("Solos")+
  ylab("AF")+
  theme_nogrid()


#e) Teste o seguinte contraste: Y1 = Argissolos e Cambissolos vs Latossolos e Neossolos.

todos<-gather(dados, key=Ys, value=valores, -AF, -blocos)

modelos<-todos %>% 
  split(dados$solos) %>% 
  map(~lm(valores~blocos+AF, data=.))



#anova(modelos$`1`) #este usado apenas para 1 tipo de solo

library(myfuns)
library(purrr)
map(modelos, anova)


medias<-emmeans(modelos)

multcomp::cld(medias, adjust="tukey", Letters=letters)


medias<-function(x) {
  
  medias<-emmeans(x, ~AF)
  mediastest<-emmeans(x)
  
  multcomp::cld(medias, adjust="tukey", Letters=letters)
}




library(dplyr)

filter(dados,(solos==1 & blocos==c(1:5)))



#f) Considere Argissolos como o controle e aplique o teste de Dunnett.

#g) Interprete os resultados.


# 3 - A produtividade (kg por parcela) de  variedades de amendoim,  sem e com aplicação de cobertura vegetal, #
#foi avaliada em um experimento realizado no DBC. Os resultados  obtidos estão contidos em dados1:

dados1<-data.frame(
  stringsAsFactors = FALSE,
                    Blocos = c(1L,2L,3L,4L,
                               1L,2L,3L,4L,1L,2L,3L,4L,1L,2L,3L,4L,
                               1L,2L,3L,4L,1L,2L,3L,4L),
                Variedades = c("IAC 503",
                               "IAC 503","IAC 503","IAC 503","IAC 505","IAC 505",
                               "IAC 505","IAC 505","IAC OL3","IAC OL3",
                               "IAC OL3","IAC OL3","IAC 503","IAC 503",
                               "IAC 503","IAC 503","IAC 505","IAC 505","IAC 505",
                               "IAC 505","IAC OL3","IAC OL3","IAC OL3",
                               "IAC OL3"),
                 Cobertura = c("Sem","Sem",
                               "Sem","Sem","Sem","Sem","Sem","Sem","Sem",
                               "Sem","Sem","Sem","Com","Com","Com","Com",
                               "Com","Com","Com","Com","Com","Com","Com",
                               "Com"),
                      Prod = c(54.48759393,
                               87.87612266,51.39583367,36.94065738,69.33387211,
                               112.7389595,70.31458377,51.52285459,
                               54.53883454,84.5885104,51.7562503,38.74659164,
                               74.38511272,114.4513472,70.6750004,53.32878886,
                               44.69255636,64.7256736,47.8375002,39.16439443,
                               77.83609478,116.3150484,112.0738829,99.85232191)
        )

#a) Obtenha o quadro da Anova, incluindo o CV.


#b) Teste a normalidade dos resíduos.
#c) Aplique o teste de Tukey para variedades e o teste F para coberturas.
#d) Apresente os resultados de c) em uma figura, utilizando ggplot2.
#e) Interprete os resultados. 


#4- No experimento citado no item 2, foi avaliada também a massa da matéria seca foliar (g por muda), cujos resultados
# estão em dados2:


dados2<-data.frame(
               solos = c(1L,1L,1L,1L,1L,2L,2L,
                         2L,2L,2L,3L,3L,3L,3L,3L,4L,4L,4L,4L,4L,5L,
                         5L,5L,5L,5L,6L,6L,6L,6L,6L),
              blocos = c(1L,2L,3L,4L,5L,1L,2L,
                         3L,4L,5L,1L,2L,3L,4L,5L,1L,2L,3L,4L,5L,1L,
                         2L,3L,4L,5L,1L,2L,3L,4L,5L),
                  AF = c(41.34322525,38.38308972,
                         38.97390626,31.63958896,36.66798454,22.78348048,
                         16.76976829,27.3451723,37.81177863,35.41822361,
                         24.12985781,18.46962888,15.44074588,28.70434713,17.29227191,
                         11.27180798,19.0075966,14.63392875,24.06689838,
                         4.014525839,15.29583545,11.09286136,6.412227478,
                         11.94540392,15.44452119,50.21190468,48.90082796,
                         54.53059803,39.81937162,47.23100813),
                 MMS = c(8.640529328,9.542507511,
                         7.865851885,8.181170941,7.44777176,5.487972877,
                         5.540613112,5.202014982,8.992242887,6.796609601,
                         4.790198769,4.786247995,1.981638317,7.631287608,4.811049368,
                         3.708239767,4.612069116,4.531818382,4.732938883,
                         0.166779522,3.460386037,1.889013718,2.082570576,
                         2.224963909,3.4721931,10.30844547,9.334119491,
                         10.4460979,9.567116664,10.30842725)
        )


# a) Utilize a função gather (ou similar) para colocar todos os valores númericos e as duas variáveis 
# dependentes (ys)  em colunas únicas.

# b) Utilize o resultado de a) para obter a ANOVA das duas variáveis SIMULTANEAMENTE.
# C) Aplique o teste de Tukey SIMULTANEAMENTE nas duas variáveis.



library(ExpDes.pt)

dbc


# $ - Determina o conunto de elementos desejados 
