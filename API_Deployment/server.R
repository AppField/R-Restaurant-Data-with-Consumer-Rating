## Expose prediction model as web service

## startup: read model

library(rjson)
load("model.rda")

decode <- function(s)
{
    as.data.frame(fromJSON(s$postBody))
}

#* @post /regression
#* @json
function(req)
{
    as.character(try(predict(m, decode(req))))
}

## add more interfaces, if needed -- e.g., for other models.
