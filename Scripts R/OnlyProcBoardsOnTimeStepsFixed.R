## necessário para realizar plot bonito dos gráficos
library (ggplot2)
## necessário para facilitar o parse da string str_extract
library (stringr)
## necessário para realizar a media e desvio padrão das amostras
library (plyr)

## Converte de joules para kWh
##
## @param x energia em joules
##
## @return o equivalente da energia em Joules
##
j2kwh <- function(x) {
    x <- x * 2.7778e-7
    x
}

##
## Recebe dois dataframes e constroi um novo
##
## @param df dataframe contendo as amostragens de consumo
## @param currentFile arquivo contendo tempo de início fim
##
## @param dataframe contendo o consumo e tempo para o experimento
##
fillData <- function(df, currentFile) {
    ## quebra o arquivo no hifen
    splitedName <- strsplit(currentFile, "-")[[1]]
    ## encontra o numero da amostra
    sampleVal <- strsplit(splitedName[4], "\\.")[[1]][1]
    ## encontra o número de processos
    procsVal <- splitedName[2]
    ## leitura do arquivo para encontrar
    ## tempo de start e finish
    times <- scan(currentFile)
    ## pode esconder casas decimais use o comando format para conferir
    ## format(times, nsmall = 40)
    start  <- times[1]
    finish <- times[2]
    ## encontra o consumo faznedo a integral de
    ## potência pela duração de tempo
    joules <- sum(df[ df$Time > start & df$Time < finish , ]$Power *
                  df[ df$Time > start & df$Time < finish , ]$Duration )
    ## finalmente encontra o dataframe e realiza 
    dataDf <- NULL
    dataDf <- data.frame(Procs=as(procsVal, "numeric"),
                         Sample=as(sampleVal, "numeric"),
                         Consumo=as(joules, "numeric"),
                         Tempo=finish-start)
    ## retorna o dataframe
    dataDf
}

##
## Recebe um arquivo csv e constroi o dataframe baseado nos arquivos de
## log respectivos
## 
## @param currentFile arquivo contendo tempo de início fim
##
## @return dataframe contendo o consumo e tempo para cada amostra e cada
##                   configuração de n. de processadores
##
computeEnergy <- function(currentFile) {

    ## leitura do primeiro csv
    df <- read.csv2(currentFile, header=TRUE, sep=";")

    ## calcula a duração entre cada período df[1..n]$Time - df[0..n-1]$Time
    ## assim pode calcular a integral e o consumo efetivo
    df["Duration"] <-  c(0,
                     df[df$Sample >= 1 & df$Sample <= max(df$Sample), ]$Time -
                     df[df$Sample >= 0 & df$Sample < max(df$Sample), ]$Time)

    
    ## expressão regular para arquivos de log
    ## pega somente arquivos para essa configuração de procs
    procs = str_extract(currentFile, "-*[0-9]+")
    regExp = paste(".+-", procs, "-.+.log$", sep="")
    ## encontra arquivos que contém início e fim
    filenames <- list.files (path = ".", pattern = regExp, full.names = TRUE)
    
    ## deve fazer o dataframe para cada arquivo de amostra, combinando em um
    ## unico dataframe no final "rbind"
    newDf <- NULL
    newDf <- do.call(rbind, lapply(filenames, function(x) fillData(df, x)))
    ## retorna o novo dataframe
    newDf
}

## extensão .csv são os arquivos contendo informação de consumo
csvRegExp = "(.+).csv$"

## encontra todos os arquivos segundo a expressão regular
csvFiles <- list.files (path = ".", pattern = csvRegExp, full.names = TRUE)

## calcula o consumo para cada configuração e amostra, rbind
## combina todos dataframes em um único para posterior análise
finalDf <-  do.call(rbind, lapply(csvFiles, computeEnergy))

## adiciona duas colunas para o tempo e consumo normalizados
finalDf["TempoNorm"] <- finalDf$Tempo/max(finalDf$Tempo)
finalDf["ConsumoNorm"] <- finalDf$Consumo/max(finalDf$Consumo)

## calcula a media, desvio padrão, erro, intervalo de confiança e variancia
## das amostras agrupando pelo numero de procs
## usa student
intconf <- 0.975
dfSumm <- data.frame(ddply(finalDf,~Procs, summarise, 
                           MConsumo=mean(Consumo), MTempo=mean(Tempo), 
                           DPConsumo=sd(Consumo), DPTempo=sd(Tempo), 
                           ERConsumo=DPConsumo/sqrt(length(Consumo)),
                           ERTempo=DPTempo/sqrt(length(Tempo)), 
                           ICConsumo= (ERConsumo * qt(intconf,length(Consumo)-1)) / sqrt(length(Consumo)),
                           ICTempo=   (ERTempo   * qt(intconf,length(Tempo)-1))   / sqrt(length(Tempo)),
                           VARConsumo=var(Consumo), VARTempo=var(Tempo) ))

dfSumm["TempoNorm"] <- dfSumm$MTempo/max(dfSumm$MTempo)
dfSumm["ConsumoNorm"] <- dfSumm$MConsumo/max(dfSumm$MConsumo)


## salva os dados
write.table(finalDf, file="finalDf.csv", append=FALSE, sep=";",col.names=TRUE, row.names=FALSE)
write.table(dfSumm, file="dfSumm.csv", append=FALSE, sep=";",col.names=TRUE, row.names=FALSE)


## define um tema próprio para os gráficos
graphScale=2.5
pedro_theme = theme(
    axis.text.x = element_text(family="Times", size = rel(graphScale-0.1*graphScale), colour="black"),
    axis.title.x = element_text(family="Times", face="bold", size = rel(graphScale), colour="black"),
    axis.text.y = element_text(family="Times", size = rel(graphScale-0.1*graphScale), colour="black"),
    axis.title.y = element_text(family="Times", face="bold", size = rel(graphScale), colour="black"),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour="grey77", linetype="dotted")
    )

fit1 <- nls(ConsumoNorm ~ a*log(TempoNorm,2) + b, start = list(a = -0.5, b = 1), data = finalDf)
summary(fit1)

power_eqn = function(df, start = list(a =1,b=-1.1)){
  m = nls(ConsumoNorm ~ a*log(TempoNorm,2) + b, start = start, data = df);
  eq = substitute(italic(y)==a~italic(x)^b, 
                   list(
                       a = format(coef(m)[1], digits = 2), 
                       b = format(coef(m)[2], digits = 2)
                       )
                   )
  as.character(as.expression(eq));       
}


## O mais próximo seria um modelo assim, porém fica difícil justificar
## Uma boa justificativa seria que o consumo permanece +/- constante
## pois quanto mais recursos menos tempo demora, ou seja, o custo depende
## mais do tempo de execução do que do consumo em si. Assim, como a aplicação
## segue uma aceleração clássica a cada processador adicionado o tempo de execu
## ção reduz pela metade porém 
#-.04*log(x-.16,2)+.52=f(x)


## mostra a energia relativa em função do tempo relativo
ggplot(finalDf, aes(x=TempoNorm,y=ConsumoNorm)) +
    geom_point(aes(size = MTempo), colour="grey34", shape=3, size=2.5*graphScale, alpha=0.4) +
    ##stat_smooth(method = 'nls', formula = 'y~a*log(x)+b', color="skyblue3", start = list(a=1,b=1),se=FALSE) +
    ##''
    stat_function(fun=function(x) -0.04*log(x-0.16,2)+0.52, colour="skyblue3") +
    xlab("\nrelative time-to-solution") +
    ylab("relative energy-to-solution\n") +
    ##geom_text(
      ##aes(x = 0.38, y = 0.9,label = as.character(as.expression(power_eqn(finalDf)))),
      ##colour="grey51", parse = TRUE) +
    xlim(0, 1) +
    ylim(0, 1) +
    pedro_theme +
    ggsave("3_ondes_tempoxconsumo-relativo.pdf")

##Plot NProc X Tempo Medio
## ggplot(finalDf, aes(x=Procs, y=Tempo)) +
##     theme_bw()  +
##     geom_point()+
##     stat_smooth() +
##     xlab("number of nodes") +
##     ylab("time-to-solution (s)") +
##     scale_x_continuous(breaks = round(seq(min(dfSumm$Procs), max(dfSumm$Procs), by = 1),1))  

##Plot NProc X Consumo Medio
## ggplot(finalDf, aes(x=Procs, y=Consumo)) +
##     theme_bw()  +
##     geom_point() +
##     stat_smooth() +
##     xlab("Processadores") +
##     ylab("Consumo de energia (J)") +
##     scale_x_continuous(breaks = round(seq(min(dfSumm$Procs), max(dfSumm$Procs), by = 1),1))  +
##     ggsave("nrpocxmconsumoIC.png")


## Mostra tempo em função do número de processadores com intervalo de confiaça
## http://stackoverflow.com/questions/16463325/shading-confidence-intervals-manually-with-ggplot2
ggplot(dfSumm, aes(Procs)) +
    geom_point(aes(y=MTempo), colour="skyblue3", size=2.5*graphScale) + 
   #geom_ribbon(aes(ymin=MTempo - ICTempo, ymax=MTempo + ICTempo), alpha=0.4) +
    geom_errorbar(aes(ymin=MTempo - ICTempo, ymax=MTempo + ICTempo),
                      width=.4,                    
                      position=position_dodge(.9)) +
    ylab("time-to-solution (s)\n") +
    xlab("\nnodes") +
    xlim(1, 10) +
    ylim(0, max(dfSumm$MTempo)+100) +
    scale_x_continuous(breaks = round(seq(min(dfSumm$Procs), max(dfSumm$Procs), by = 1),1)) +
    pedro_theme + 
    ggsave("3_ondes_tempoxprocs.pdf", width=10,height=6.2)

## Mostra consumo em função do número de processadores com intervalo de confiaça
ggplot(dfSumm, aes(Procs)) +
    geom_point(aes(y=MConsumo), colour="skyblue3", size=2.5*graphScale) + 
    #geom_ribbon(aes(ymin=MConsumo - ICConsumo, ymax=MConsumo + ICConsumo), alpha=0.4) +
    geom_errorbar(aes(ymin=MConsumo - ICConsumo, ymax=MConsumo + ICConsumo),
                      width=.4,
                      position=position_dodge(.9)) +
    ylab("energy-to-solution (joules)\n") +
    xlab("\nnodes") +
    xlim(1, 10) +
    ylim(0, 30000+100) +
    pedro_theme +
    scale_x_continuous(breaks = round(seq(min(dfSumm$Procs), max(dfSumm$Procs), by = 1),1)) +
    ggsave("3_ondes_consumoxprocs.pdf", width=10,height=6)


#PLOT kJ
    ggplot(dfSumm, aes(Procs)) +
    geom_point(aes(y=MConsumo/1000), colour="skyblue3", size=2.5*graphScale) + 
    #geom_ribbon(aes(ymin=MConsumo - ICConsumo, ymax=MConsumo + ICConsumo), alpha=0.4) +
    geom_errorbar(aes(ymin=MConsumo/1000 - ICConsumo/1000, ymax=MConsumo/1000 + ICConsumo/1000),
                      width=.4,
                      position=position_dodge(.9)) +
    ylab("energy-to-solution (kJ)\n") +
    xlab("\nnodes") +
    xlim(1, 10) +
    ylim(0, 31) +
    pedro_theme +
    scale_x_continuous(breaks = round(seq(min(dfSumm$Procs), max(dfSumm$Procs), by = 1),1)) +
    ggsave("3_ondes_consumoxprocs.pdf", width=10,height=6.2)


## faz um plot clean do da media dos resultados, usa uma 
##terceira dimensão onde o tamanho de cada ponto representa o tempo
## ggplot(dfSumm, aes(TempoNorm,ConsumoNorm)) +
##     theme_bw()  +
##     geom_point(aes(size = MTempo)) +
##     xlab("Tempo relativo") +
##     ylab("Consumo de energia relativo") +
##     xlim(0, 1) +
##     ylim(0, 1) +
##     ggsave("temporelxconsrelxtempo.png")

## ##Plot Consumo Medio, Tempo Medio e NProc - FIXME ordenação 1 10 2 .... arrange não funcionou
## ##cor pode ser alpha ou shape
## ggplot(dfSumm, aes(MConsumo,MTempo)) +
##     theme_bw()  +
##     geom_point(aes(colour = Procs)) +
##     xlab("Consumo de energia (J)")  +
##     ylab("Tempo (s)") +
##     ggsave("tempoxconsumomed.png")

## ##Plot Consumo, Tempo e NProc para todas as amostras
## ggplot(finalDf, aes(Consumo,Tempo)) +
##     theme_bw()  +
##     geom_point(aes(colour = Procs)) +
##     xlab("Consumo de energia (J)")  +
##     ylab("Tempo (s)") +
##     ggsave("tempoxconsumo.png")



## Grafico legal se fosse multicore - tipo panda
##http://www.statmethods.net/advgraphs/images/ggplotcustomized.png
#qplot(Tempo, Consumo, data=finalDf, 
#   facets=Procs~Sample, main="Scatterplots of Procs vs. Samples",
#   xlab="Tempo", ylab="Consumo")

