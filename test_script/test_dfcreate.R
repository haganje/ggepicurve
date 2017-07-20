z %>%
arrange(dates_evenements, groupe_facet, ordre_periode_facet) %>%
select(dates_evenements,  periode, ordre_periode, groupe_facet, ordre_periode_facet)

z <- prepare_df(dates = outbreak_sim$dates, groupe_couleur = outbreak_sim$yard, groupe_facet = outbreak_sim$type)

prepare_df("aes")
prepare_df(c(as.Date(c("2017-01-01", NA))))
