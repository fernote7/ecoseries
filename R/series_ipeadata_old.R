#' A function to extract Sidra series using their API
#' @param arg1 ipea series number.
#' @param ... More series number.
#' @param periodicity A string specifying the periodicity.
#' @param save A string specifying if data should be saved in csv or xlsx format. 
#' Defaults to not saving.
#' @keywords ipea ipeadata
#' @import rvest xml2 stats


#ipea2=series_ipeadata_old(394221910, periodicity = "Y") 

series_ipeadata_old <- function(arg1, ..., periodicity = "D", save = ""){
    
    #arg1 = 394221910; arg2 = 40940; periodicity = "D"; save=""; i =1
    
    inputs = as.character(list(arg1, ...))
    #inputs = as.character(list(arg1, arg2))
    len = seq_along(inputs)
    serie = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)
    
    for (i in len){
        
        pagina <- xml2::read_html(paste0("http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=", 
                                   inputs[i], "&module=M"))
    
        dados<-pagina %>%
            rvest::html_nodes(".dxgvDataRow")%>%
            rvest::html_text()
        
        dados<-gsub("\r\n\t\t\t", "", dados)

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
        assign(serie[i], dat)
    }
    
    rm(dat)

    if (save != ""){
        if (save == "csv"){
            for(i in len) {utils::write.csv(eval(as.symbol(serie[i])), file = paste0(serie[i], ".csv"))}
        } else if (save == "xls" | save == "xlsx") {
            for(i in len) {write.xlsx(eval(as.symbol(serie[i])), file = paste0(serie[i], ".xlsx"), 
                                      row.names = FALSE)}} else{ 
                                          stop("save argument must be 'csv' or 'xlsx' ")}
    }
    
    lista = list()
    ls_df = ls()[grepl('data.frame', sapply(ls(), function(x) class(get(x))))]
    for ( obj in ls_df ) { lista[obj]=list(get(obj)) }
    
    return(invisible(lista))
}