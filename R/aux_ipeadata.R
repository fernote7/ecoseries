#' A function to extract BACEN series using their API
#' @param arg1 Bacen series number.
#' @param periodicity A string specifying the periodicity.
#' @keywords bacen
#' @import rvest xml2 magrittr

aux_ipeadata <- function(arg1, periodicity){

    #arg1 = 394221910; arg2 = 40940; periodicity = "D"; save=""; i =1
    
    inputs = as.character(list(arg1))

    pagina <- xml2::read_html(paste0("http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=", 
                               inputs, "&module=M"))
    
    dados<-pagina %>%
        rvest::html_nodes(".dxgvDataRow")%>%
        rvest::html_text()
    
    dados<-gsub("\r\n\t\t\t", "", dados)
    #dados<-gsub("[[:punct:]]","",dados)
    
    if (periodicity == "D"){
        data<-substr(dados, 1, 10)
        valor<-substr(dados, 11, 100)
    } else if (periodicity == "Y"){
        data<-substr(dados, 1, 4)
        valor<-substr(dados, 5, 100)
    } else if (periodicity == "M"){
        data<-substr(dados, 1, 7)
        valor<-substr(dados, 8, 100)
    } else { stop("Wrong periodicity. This field accepts 'Y', 'M' or 'D' as arguments.")}
    
    valor = gsub("\\.", "", valor)
    valor = gsub(",", ".",valor)
    valor = as.numeric(valor)
    dat <- data.frame(data,valor)
    dat <- dat[stats::complete.cases(dat),]

    return(dat)
}