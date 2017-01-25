#' A function to extract Sidra series using their API
#' 
#' The different parameters define a table and its dimensions (periods, variables, territorial units and classification) to be consulted. The parameters that define the territorial units can be used more than once, allowing multiple distinct territorial searches with one request. The parameters that define the classes may vary from table to table, with a maximum of 6. Henceforth, a Sidra table ranges between 3 and 9 differente dimension (3 mandatory - periods, var and terr and 6 optional).
#' @param x Sidra series number.
#' @param from A string specifying where the series shall start. Defaults to 1980.
#' @param to A string specifying where the series shall end. Defaults to current year.
#' @param territory Specifies the desired territorial levels.
#' @param header Logical. Either TRUE or FALSE.
#' @param save A string specifying if data should be saved in csv or xlsx format. 
#' Defaults to not saving.
#' @keywords sidra
#' @export
#' @import RCurl rjson
#' @examples
#' sidra=series_sidra(x = c(1612), from = "", to = "", territory = "brazil")
#' sidra=series_sidra(x = c(3653, 3651, 3652), from = "200201", to = "201512", territory = "brazil", secoes = list(c(544,129316,129330), c(543,129282,129299)))


series_sidra <- function(x, from = "", to = "", territory = c(n1 = "brazil", n2 = "region", n3 = "state"), header = TRUE, save = "", secoes = NULL){
    
    x = as.character(x)
    
    
    if (from == ""){
        data_init = "1980"
    } else {data_init = from}
    
    if (to == ""){
        data_end = format(Sys.Date(), "%Y")
    } else {data_end = to}
    
    
    # Território
    territory <- base::match.arg(territory)
    territory <- base::switch(territory,
                              brazil = "n1/all", 
                              region = "n2/all", 
                              state = "n3/all")
    
    if ( header == TRUE | header == T) { 
        header = "y"
    } else if (header == FALSE | header == F) { 
        header = "n"
    } else { stop("header assume only TRUE or FALSE values")}
    
    
    
    
    
    if (! is.null(secoes)){
        for (i in seq_along(secoes)){
        
        secoes[i] = paste0("/c", secoes[[i]][1], "/", 
                           paste0(secoes[[i]][2:length(secoes[[i]])], collapse = ","))
        }
    }
    secoes = c(secoes, rep('', length(x)-length(secoes)))
    
    
    
    inputs = as.character(x)
    len = seq_along(inputs)
    serie = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)
    
    for (i in len){
        tabela=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                                    "t/", inputs[i], "/", territory, "/", "p/", 
                                    data_init, "-", data_end,  
                                    "/v/", "allxp", "/f/", "u", "/h/", header,
                                    secoes[[i]]),
                             ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
        
        # http://api.sidra.ibge.gov.br/values/t/3653/n3/all/p/200501-201612/v/allxp/f/u/h/y/C544/129314,129315    
        # t/3653/f/c/h/n/n1/all/V/allxp/P/all/C544/129314,129315/d/s    
        #    t/3653/n3/all/p/200501-201612/C544/129314,129315/v/allxp/f/u/h/y 
        
        
        if (strsplit(tabela, " ")[[1]][1] == "Par\uE2metro") {
            
            stop("The parameters 'from', 'to' or both are misspecified")
            
            
        } else if (strsplit(tabela, " ")[[1]][1] == "Tabela" & 
                   strsplit(tabela, " ")[[1]][3] == "Tabela"){
            
            param = strsplit(tabela, " ")[[1]][2]
            param = substr(param, 1, nchar(param)-1)
            warning(sprintf("The table %s does not contain public data", param))
            
        } else{
            t1 = paste("tabela", x, sep="_")
            tabela = rjson::fromJSON(tabela)
            tabela=data.frame(do.call("rbind", tabela))
            if (header == "y"){
                
                colnames(tabela) = unlist(tabela[1,])
                tabela = tabela[-1,]
            }
            
            #Transformando a coluna V em valor
            
            valor = NULL
            
            id = which(colnames(tabela)=="V" | colnames(tabela)=="Valor")
            
            #tabela2 = tabela
            #tabela2[tabela2[,id] ==  "..",id] <- NA
            #tabela2[,id] <- as.numeric(tabela2[,id])
            tabela[,id] = suppressWarnings(ifelse(tabela[,id]!="..", 
                                                  as.numeric(tabela[,id]),NA))
        }
        
        assign(serie[i],tabela)
        rm(tabela)
    }
    
    lista = list()
    ls_df = ls()[grepl('data.frame', sapply(ls(), function(x) class(get(x))))]
    for ( obj in ls_df ) { lista[obj]=list(get(obj)) }
    
    return(invisible(lista))
    
}
