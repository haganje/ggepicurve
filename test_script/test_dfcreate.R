z %>%
arrange(dates_evenements, groupe_facet, ordre_periode_facet) %>%
select(dates_evenements,  periode, ordre_periode, groupe_facet, ordre_periode_facet)

z <- prepare_df(dates = outbreak_sim$dates, group_color = outbreak_sim$yard, group_facet_vertical = outbreak_sim$type)

plot_ggcurve(z)

z <- prepare_df(dates = outbreak_sim$dates, group_color = outbreak_sim$yard, group_facet_horizontal= strftime(outbreak_sim$dates, format = "%Y"))
plot_ggcurve(z, period_numeric = T)

prepare_df("aes")
prepare_df(c(as.Date(c("2017-01-01", NA))))
