#' A function to extract Sidra series using their API
#' 
#' The different parameters define the table and its dimensions (periods, variables, territorial units and classification) to be consulted. The parameters that define the sections may vary from table to table. Henceforth, the Sidra function ranges between 2 mandatory arguments - x (the series number) and territory (the geographic scope) to 6 arguments, where you can input the time window wanted, the variables and the sections. You can only choose one variable per series per request, but multiple sections within the variable.
#' @param x Sidra series number.
#' @param from A string or character vector specifying where the series shall start. Defaults to 1980.
#' @param to A string or character vector specifying where the series shall end. Defaults to current year.
#' @param territory Specifies the desired territorial levels.
#' @param header Logical. Either TRUE or FALSE.
#' @param save A string specifying if data should be saved in csv or xlsx format. 
#' Defaults to not saving.
#' @param variable An integer describing what variable characteristics are to be returned. Defaults to all available.
#' @param sections A vector containing the classification code in the first slot and the desired tables from this classification.
#' @keywords sidra
#' @export
#' @import RCurl rjson
#' @examples
#' sidra=series_sidra(x = c(1612), from = NULL, to = NULL, territory = "brazil")
#' sidra=series_sidra(x = c(3653), from = c("200201"), 
#' to = c("201512"), territory = "brazil", 
#' variable = 3135, sections = list(c(544,129316,129330)))


series_sidra <- function(x, from = NULL, to = NULL, territory = c(n1 = "brazil", n2 = "region", n3 = "state"), header = TRUE, save = "", variable = "allxp", sections = NULL){
    
    x = as.character(x)
    
    
    if (is.null(from)){
        data_init = rep("1980", length(x))
    } else if (length(from == 1)) {
        data_init = rep(from, length(x))
    }else {data_init = as.character(from)}
    
    if (is.null(to)){
        data_end = rep(format(Sys.Date(), "%Y"), length(x))
    } else if (length(to == 1)) {
        data_end = rep(to, length(x))
    }else {data_end = as.character(to)}
    
    if (variable == "allxp"){
        variable = rep(variable, length(x))
    }
        
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
    
    
    
    
    
    if (! is.null(sections)){
        for (i in seq_along(sections)){
        
        sections[i] = paste0("/c", sections[[i]][1], "/", 
                           paste0(sections[[i]][2:length(sections[[i]])], 
                                  collapse = ","))
        }
    }
    sections = c(sections, rep('', length(x)-length(sections)))
    
    
    
    inputs = as.character(x)
    len = seq_along(inputs)
    serie = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)
    
    for (i in len){
        tabela=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                                    "t/", inputs[i], "/", territory, "/", "p/", 
                                    data_init[i], "-", data_end[i],  
                                    "/v/", variable[i], "/f/", "u", "/h/", header,
                                    "/", sections[[i]]),
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
