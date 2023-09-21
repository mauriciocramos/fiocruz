library("elastic");

connect(es_host = "elasticsearch.icict.fiocruz.br",  es_port = 8201,
        es_user = "r_user",es_pwd = "r_user", es_transport_schema = "https");


consulta<-

  install.packages("read.dbc", dependencies = T)
  library("read.dbc")
    
consulta<-'{
"size": 0,
"query": {
"range": {
"GESTACAO":{
                "gte" : 1,
                "lte" : 4,
                "boost" : 2.0
            }
}
},
"aggs": {
"2": {
"terms": {
"field": "ano_nasc",
"size": 15,
"order": {
"_term": "desc"
}
}
}
}
}'


consulta2<-'{
          "size": 0,
"query": {
"range": {
"GESTACAO":{
                "gte" : 1,
                "lte" : 6,
                "boost" : 2.0
            }
}
},
"aggs": {
"2": {
"terms": {
"field": "ano_nasc",
"size": 15,
"order": {
"_term": "desc"
}
}
}
}
}'

numerador<-Search(index="datasus-sinasc", size = 0, asdf = TRUE, body = consulta)

numerador2<-Search(index="datasus-sinasc", size = 0, asdf = TRUE, body = consulta2)

numerador$aggregations$`2`$buckets$key
numerador$aggregations$`2`$buckets$doc_count

dataset<-data.frame(numerador$aggregations$`2`$buckets$key, numerador$aggregations$`2`$buckets$doc_count, numerador2$aggregations$`2`$buckets$doc_count)
names(dataset)<-c("ano","menor_36","totais")

View(dataset)


plot(x = dataset$ano, y = dataset$totais, type = c("l"))

dataset$pct_premat<-dataset$menor_36/dataset$totais*100
plot(x = dataset$ano, y = dataset$pct_premat, type = c("l"))