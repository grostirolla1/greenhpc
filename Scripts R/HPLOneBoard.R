

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
