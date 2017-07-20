#' Ordre d'une observation pour une semaine
#'
#' @description Calcul pour chaque observation son rang en fonction de la semaine et du type de cas (optionel).
#'
#' @param date_evenement Vecteur de dates de l'événement
#' @param limite_temporelle Limite temporelle, par défaut semaine. Valeurs possibles : voir arguement format dans strftime
#' @return Vecteur numérique de même longeur que le vecteur de dates avec le rang pour chaque periode

range_periode <- function(date_evenement, limite_temporelle = "%W") {

  # Donner un ordre aux observations (pour éviter problème si le vecteur de
  # de date n'est pas trié)
  ordre_observations <- order(date_evenement)

  # Transformer les dates en numéro de semaine
  periodes <- strftime(x = date_evenement, format = limite_temporelle)

  # Calculer l'ordre pour chaque semaine
  rangs <- aggregate(x = ordre_observations, by = list(periode = periodes), order)

  # Retourner un vecteur
  unlist(rangs$x)
}

range_periode_type <- function(date_evenement, groupe, limite_temporelle = "%W") {
  # Créer une df avec un id par date
  id <- seq_len(length(date_evenement))

  df <- data.frame(
    date_evenement,
    id ,
    groupe,
    rang_groupe = NA_integer_
  )

  # Trier par date + groupe
  df_ordre <- df[order(df$groupe, df$date_evenement),]

  #Pour chaque groupe, donner un rang
  groupes <- unique(groupe)


  for(un_groupe in groupes) {
    df$rang_groupe[groupe == un_groupe] <- range_periode(
      df$date_evenement[groupe == un_groupe],
      limite_temporelle = limite_temporelle
      )
  }

  # retourner ordres triés
  df$rang_groupe[order(df$id)]
}


prepare_df <- function(dates_evenements, groupe_couleur, groupe_facet = NA, limite_temporelle = "%W", id = NULL) {
  if(is.null(id))
    id <- order(dates_evenements)

  ordre_periode <- range_periode(dates_evenements, limite_temporelle)

  # Par semaine
  periode <- as.integer(strftime(x = dates_evenements ,format = limite_temporelle))
  annee <- as.integer(strftime(x = dates_evenements, format = "%Y"))


  ordre_periode_facet <- range_periode_type(date_evenement = dates_evenements, groupe = groupe_facet)
# df_raw$week <- factor(as.integer(semaines), levels = seq_len(max(semaines)))
  data.frame(
    id,
    dates_evenements,
    periode,
    annee,
    groupe_couleur,
    groupe_facet,
    ordre_periode,
    ordre_periode_facet
  )
}


set_panel_size <- function(p=NULL, g=ggplotGrob(p), file=NULL,
                           margin = unit(1,"mm"),
                           width=unit(4, "cm"),
                           height=unit(4, "cm")){

  panels <- grep("panel", g$layout$name)
  panel_index_w<- unique(g$layout$l[panels])
  panel_index_h<- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)

    g$widths[panel_index_w] <-  rep(width,  nw)
    g$heights[panel_index_h] <- rep(height, nh)


  g
}
