#' @import rvest xml2 magrittr tibble

aux_ipeadata <- function(arg1, periodicity){

    #arg1 = 394221910; arg2 = 40940; periodicity = "D"; save=""; i =1; arg1 = 1256135866
    
    inputs = as.character(list(arg1))

    pagina = xml2::read_html(paste0("http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=", 
                               inputs, "&module=M"))
    
    dados=pagina %>%
        rvest::html_nodes("td.dxgv")%>%
        rvest::html_text()
    
    # dados=gsub("", "", dados)
    #dados=gsub("[[:punct:]]","",dados)
    
    dados <- dados[nchar(dados)>0]
    even_indexes<-seq(2,length(dados),2)
    odd_indexes<-seq(1,length(dados),2)
    
    t0 <- dados[odd_indexes]
    t1 <- dados[even_indexes]
    t_fin <- as.data.frame(cbind(t0,t1))
    
    
    
    if (periodicity == "D"){
        t_fin[,1] <- as.Date(t_fin[,1], format = "%d/%m/%Y")
    } else if (periodicity == "Y"){
        t_fin[,1] <- as.Date(paste(t_fin[,1], 1, 1, sep = "-"))
    } else if (periodicity == "M"){
        t_fin[,1] <- as.Date(paste(substr(t_fin[,1], 1,4),substr(t_fin[,1], 6,7), 1, sep = "-"))
    } else { stop("Wrong periodicity. This field accepts 'Y', 'M' or 'D' as arguments.")}
    
    valor = gsub("\\.", "", t_fin[,2])
    valor = gsub(",", ".",valor)
    valor = as.numeric(valor)
    dat = tibble::tibble(t_fin[,1],valor)
    dat = dat[stats::complete.cases(dat),]
    colnames(dat) <- c("data", "valor")

    return(dat)
}
