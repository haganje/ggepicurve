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


#' Create an object df_epidemic
#'
#' @param dates_evenements Vector of dates
#' @param groupe_couleur Vector of character
#' @param groupe_facet Vector of character
#' @param limite_temporelle Character: either NA (days), "%W" (weeks) or "%m" (month)
#' @param id Vector of character. The id of each obs. If NULL, then id will be autoassigned in the order of the dates.
#'
#' @return A data.frame
prepare_df <- function(dates_evenements, groupe_couleur, groupe_facet = NA, limite_temporelle = "%W", id = NULL) {
  if(is.null(id))
    id <- order(dates_evenements)

  ordre_periode <- range_periode(dates_evenements, limite_temporelle)

  # Par semaine
  periode <- as.integer(strftime(x = dates_evenements ,format = limite_temporelle))
  annee <- as.integer(strftime(x = dates_evenements, format = "%Y"))


  ordre_periode_facet <- range_periode_type(date_evenement = dates_evenements, groupe = groupe_facet)
# df_raw$week <- factor(as.integer(semaines), levels = seq_len(max(semaines)))
  df <- data.frame(
    id,
    dates_evenements,
    periode,
    annee,
    groupe_couleur,
    groupe_facet,
    ordre_periode,
    ordre_periode_facet
  )

  class(df) <- c(class(df), "epidemic_df")

  df
}

#' Make a ggplot from outbreak data
#'
#' @param df A data.frame tailored with the prepare_df function
#' @return A ggplot object
#' @import ggplot2

plot_ggcurve <- function(df){
  ggp <- ggplot(df) +
    aes(x = periode, fill = as.factor(groupe_couleur), y = ordre_periode_facet, label = id) +
    # scale_x_discrete(name = "Semaine", drop = FALSE) +
    geom_label(show.legend = TRUE, vjust = "top", size = 4) +
    scale_y_continuous(
      name = "Nombre de cas\n par semaine",
      limits = c(0, max(df$ordre_periode_facet)),
      minor_breaks = NULL
    ) +
    scale_x_continuous(
      name = "Semaine",
      limits = c(1, max(df$periode)),
      minor_breaks = seq_len(max(df$periode)),
      breaks = c(1, seq(from = 5, to = max(df$periode), by = 5))
    ) +
    facet_grid(groupe_facet~annee) +
    ggtitle("Courbe épidémique")

  ggp
}
