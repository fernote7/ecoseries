require(httr)
require(XML)

series_fred <- function(){
    
    
    
    apikey = "68e6a4b0b67b983afc2590ec38cc6ea1"
    
    
    teste = RCurl::getURL('https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=68e6a4b0b67b983afc2590ec38cc6ea1')
    
    
    tt1=XML::xmlParse(teste)
    xml_data <- XML::xmlToList(tt1)
    unlista = unlist(xml_data)
    dados <- matrix(unlista, ncol = 90, nrow = 4)
    dados <- t(dados)
    
    
    
    teste2 = httr::GET('https://api.stlouisfed.org/fred/series/search?api_key=68e6a4b0b67b983afc2590ec38cc6ea1&search_text=canada')
    

    t2 = httr::content(teste2)
    nodulo=unlist(t2$node)
    
    
    
    
    
    
    
    
    teste = RCurl::getURL('https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=68e6a4b0b67b983afc2590ec38cc6ea1&file_type=json')
    
    tt1 <- rjson::fromJSON(teste)
    tt1$observations
    
    
    
    
}