# =============================================================================
# Figures (PNG) from output/sim_results.rds
#
# Language: English by default, Catalan or Spanish with
#   Rscript R/02_figures.R ca   (or es; or options(escrutini.lang = "ca"))
# English figures go to output/figures/, the others to output/figures_<lang>/.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales)
})

args <- commandArgs(trailingOnly = TRUE)
lang <- if (length(args)) args[1] else getOption("escrutini.lang", "en")
stopifnot(lang %in% c("en", "ca", "es"))

S <- readRDS("output/sim_results.rds")
res <- copy(S$results); fin <- copy(S$finals); cfg <- S$cfg
out_dir <- if (lang == "en") "output/figures" else paste0("output/figures_", lang)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Texts -------------------------------------------------------------------
TX <- list(
  en = list(
    el = c("2016-06" = "Jun 2016", "2019-04" = "Apr 2019",
           "2019-11" = "Nov 2019", "2023-07" = "Jul 2023"),
    el_long = c("2016-06" = "June 2016", "2019-04" = "April 2019",
                "2019-11" = "November 2019", "2023-07" = "July 2023"),
    method = c("Raw count" = "Raw partial count", "Weighted" = "Weighted to previous election"),
    x = "Booths reported (% of all booths)",
    caption = paste0(
      "Simulation with booth-level results (Ministerio del Interior). Booths report in order: small villages first, ",
      "then towns, big cities last.\n",
      "Weighted: counted booths are reweighted so that their previous-election results match the province's previous ",
      "result.\nLines: median of %d simulated counting orders; bands: 10-90%%."),
    caption5 = paste0(
      "Simulation with booth-level results (Ministerio del Interior). Booths report in order: ",
      "small villages first, then towns, big cities last.\n",
      "Weighted: counted booths are reweighted so that their previous-election results match ",
      "the province's previous result.\nMedian of %d simulated counting orders."),
    tiers = c("Villages (< 5,000 electors)", "Small & medium towns (5,000-100,000)",
              "Large cities (> 100,000)", "All votes cast"),
    f1_title = "The count is not a random sample",
    f1_sub = "Share of each type of booth already reported, as the national count advances (simulated order, July 2023)",
    f1_y = "Already reported (%)",
    f1_cap = paste0("Reporting order: log10(electors in municipality) + ballots in booth / 1,000 + random noise.\n",
                    "When half the booths are in, far less than half of the votes have been counted."),
    f2_title = "General election, %s: vote share as the count progresses",
    f2_sub = "Dashed line = final result. The weighted estimate settles near it long before the raw count does.",
    f2_y = "Vote share (% of valid votes)",
    f2_panel = "%s  (final: %s)",
    f3_title = "How far is the partial result from the final one?",
    f3_sub = "Largest error among the main parties (>= 5% of the vote), in percentage points. Dashed line = 1 pp.",
    f3_y = "Largest error (pp)", pp = " pp",
    f4_title = "Seats in the wrong hands",
    f4_sub = "Seats allocated to a different party than in the final result (D'Hondt by province on the partial / weighted votes)",
    f4_y = "Misallocated seats (of 350)",
    f5_title = "How early could the final result be called?",
    f5_sub = "Share of booths that must be reported before the estimate stays reliable until the end (median of simulations)",
    f5_lab = "%s (%s of votes)",
    metric = c(share = "Every main party within 1 pp of its final share",
               seats = "No more than 3 seats misallocated"),
    dec = "."),
  ca = list(
    el = c("2016-06" = "juny 2016", "2019-04" = "abril 2019",
           "2019-11" = "nov. 2019", "2023-07" = "juliol 2023"),
    el_long = c("2016-06" = "juny de 2016", "2019-04" = "abril de 2019",
                "2019-11" = "novembre de 2019", "2023-07" = "juliol de 2023"),
    method = c("Raw count" = "Escrutini parcial (sense ponderar)",
               "Weighted" = "Ponderat amb les eleccions anteriors"),
    x = "Meses escrutades (% del total)",
    caption = paste0(
      "Simulació amb resultats per mesa (Ministeri de l'Interior). Les meses arriben en aquest ordre: primer els pobles petits, ",
      "després les ciutats mitjanes i al final les grans ciutats.\n",
      "Ponderat: les meses escrutades es reponderen perquè els seus resultats a les eleccions anteriors reprodueixin ",
      "el resultat anterior de la província.\nLínies: mediana de %d ordres d'escrutini simulats; bandes: 10-90%%."),
    caption5 = paste0(
      "Simulació amb resultats per mesa (Ministeri de l'Interior). Les meses arriben en aquest ordre: ",
      "primer els pobles petits, després les ciutats mitjanes i al final les grans ciutats.\n",
      "Ponderat: les meses escrutades es reponderen perquè els seus resultats a les eleccions anteriors ",
      "reprodueixin el resultat anterior de la província.\nMediana de %d ordres d'escrutini simulats."),
    tiers = c("Pobles (< 5.000 electors)", "Ciutats petites i mitjanes (5.000-100.000)",
              "Grans ciutats (> 100.000)", "Total de vots emesos"),
    f1_title = "L'escrutini no és una mostra aleatòria",
    f1_sub = "Percentatge de meses de cada tipus ja escrutades a mesura que avança el recompte (ordre simulat, juliol de 2023)",
    f1_y = "Ja escrutat (%)",
    f1_cap = paste0("Ordre d'arribada: log10(electors del municipi) + paperetes de la mesa / 1.000 + soroll aleatori.\n",
                    "Quan s'han escrutat la meitat de les meses, s'ha comptat bastant menys de la meitat dels vots."),
    f2_title = "Eleccions generals de %s: percentatge de vot durant l'escrutini",
    f2_sub = "Línia discontínua = resultat final. L'estimació ponderada s'hi acosta molt abans que l'escrutini sense ponderar.",
    f2_y = "Vot (% sobre vots vàlids)",
    f2_panel = "%s  (final: %s)",
    f3_title = "Fins a quin punt s'allunya el resultat parcial del final?",
    f3_sub = "Error més gran entre els partits principals (>= 5% del vot), en punts percentuals. Línia discontínua = 1 punt.",
    f3_y = "Error més gran (punts)", pp = " p.",
    f4_title = "Escons en mans equivocades",
    f4_sub = "Escons assignats a un partit diferent del resultat final (D'Hondt per província sobre el vot parcial o ponderat)",
    f4_y = "Escons mal assignats (de 350)",
    f5_title = "Quan es podria anticipar el resultat final?",
    f5_sub = "Percentatge de meses escrutades a partir del qual l'estimació ja no s'allunya del resultat final (mediana de les simulacions)",
    f5_lab = "%s (%s dels vots)",
    metric = c(share = "Tots els partits principals a menys d'1 punt del seu resultat final",
               seats = "No més de 3 escons mal assignats"),
    dec = ","),
  es = list(
    el = c("2016-06" = "jun. 2016", "2019-04" = "abr. 2019",
           "2019-11" = "nov. 2019", "2023-07" = "jul. 2023"),
    el_long = c("2016-06" = "junio de 2016", "2019-04" = "abril de 2019",
                "2019-11" = "noviembre de 2019", "2023-07" = "julio de 2023"),
    method = c("Raw count" = "Escrutinio parcial (sin ponderar)",
               "Weighted" = "Ponderado con las elecciones anteriores"),
    x = "Mesas escrutadas (% del total)",
    caption = paste0(
      "Simulación con resultados por mesa (Ministerio del Interior). Las mesas llegan en este orden: primero los pueblos pequeños, ",
      "después las ciudades medianas y al final las grandes ciudades.
",
      "Ponderado: las mesas escrutadas se reponderan para que sus resultados en las elecciones anteriores reproduzcan ",
      "el resultado anterior de la provincia.
Líneas: mediana de %d órdenes de escrutinio simulados; bandas: 10-90%%."),
    caption5 = paste0(
      "Simulación con resultados por mesa (Ministerio del Interior). Orden de llegada: ",
      "primero pueblos pequeños, después ciudades medianas y al final grandes ciudades.
",
      "Ponderado: las mesas escrutadas se reponderan para que sus resultados en las elecciones anteriores ",
      "reproduzcan el resultado anterior de la provincia.
Mediana de %d órdenes de escrutinio simulados."),
    tiers = c("Pueblos (< 5.000 electores)", "Ciudades pequeñas y medianas (5.000-100.000)",
              "Grandes ciudades (> 100.000)", "Total de votos emitidos"),
    f1_title = "El escrutinio no es una muestra aleatoria",
    f1_sub = "Porcentaje de mesas de cada tipo ya escrutadas a medida que avanza el recuento (orden simulado, julio de 2023)",
    f1_y = "Ya escrutado (%)",
    f1_cap = paste0("Orden de llegada: log10(electores del municipio) + papeletas de la mesa / 1.000 + ruido aleatorio.
",
                    "Con la mitad de las mesas escrutadas, se ha contado bastante menos de la mitad de los votos."),
    f2_title = "Elecciones generales de %s: porcentaje de voto durante el escrutinio",
    f2_sub = "Línea discontinua = resultado final. La estimación ponderada se acerca a él mucho antes que el escrutinio sin ponderar.",
    f2_y = "Voto (% sobre votos válidos)",
    f2_panel = "%s  (final: %s)",
    f3_title = "¿Cuánto se aleja el resultado parcial del final?",
    f3_sub = "Mayor error entre los partidos principales (>= 5% del voto), en puntos porcentuales. Línea discontinua = 1 punto.",
    f3_y = "Mayor error (puntos)", pp = " p.",
    f4_title = "Escaños en manos equivocadas",
    f4_sub = "Escaños asignados a un partido distinto del resultado final (D'Hondt por provincia sobre el voto parcial o ponderado)",
    f4_y = "Escaños mal asignados (de 350)",
    f5_title = "¿Cuándo se podría anticipar el resultado final?",
    f5_sub = "Porcentaje de mesas escrutadas a partir del cual la estimación ya no se aleja del resultado final (mediana de las simulaciones)",
    f5_lab = "%s (%s de los votos)",
    metric = c(share = "Todos los partidos principales a menos de 1 punto de su resultado final",
               seats = "No más de 3 escaños mal asignados"),
    dec = ",")
)
T <- TX[[lang]]
pct_lab <- function(x, acc = 1) label_percent(scale = 1, accuracy = acc, decimal.mark = T$dec,
                                              big.mark = if (lang == "en") "," else ".")(x)

# ---- Style -------------------------------------------------------------------
col_method <- c("Raw count" = "#eb6834", "Weighted" = "#2a78d6")
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"

theme_es <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.background  = element_rect(fill = "white", colour = NA),
          panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
          panel.grid.minor = element_blank(),
          text = element_text(colour = ink),
          axis.text = element_text(colour = ink2),
          plot.title = element_text(face = "bold", size = base + 3),
          plot.subtitle = element_text(colour = ink2, margin = margin(b = 8)),
          plot.caption = element_text(colour = ink2, size = base - 3, hjust = 0),
          plot.title.position = "plot", plot.caption.position = "plot",
          strip.text = element_text(face = "bold", hjust = 0, size = base),
          legend.position = "top", legend.justification = "left",
          legend.title = element_blank())
}
save_png <- function(p, file, w = 10, h = 6) {
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 300,
         device = ragg::agg_png, bg = "white")
}
caption_txt <- sprintf(T$caption, cfg$n_reps)
sc_method <- list(scale_colour_manual(values = col_method, labels = T$method),
                  scale_fill_manual(values = col_method, labels = T$method))

res[, el := factor(T$el[election], levels = T$el)]
fin[, el := factor(T$el[election], levels = T$el)]

# Main parties: >= 5% of the final national vote
main <- fin[share >= 5, .(election, party, final_share = share, final_seats = seats)]

# ---- Error metrics -----------------------------------------------------------
err <- merge(res, main, by = c("election", "party"))
err_sum <- err[, .(max_err = max(abs(share - final_share))),
               by = .(election, el, method, rep, pct_booths, pct_votes)]

seat_err <- merge(res, fin[, .(election, party, final_seats = seats)],
                  by = c("election", "party"), all.x = TRUE)
seat_err[is.na(final_seats), final_seats := 0][is.na(seats), seats := 0]
seat_err <- seat_err[, .(seats_off = sum(abs(seats - final_seats)) / 2),
                     by = .(election, el, method, rep, pct_booths, pct_votes)]

band <- function(d, v) d[, .(med = median(get(v)), lo = quantile(get(v), 0.1),
                             hi = quantile(get(v), 0.9), pct_votes = median(pct_votes)),
                         by = .(el, method, pct_booths)]

# First % of booths after which the error stays below a threshold for good
time_to <- function(d, v, thr) {
  d <- d[order(pct_booths)]
  d[, ok_after := rev(cummax(rev(get(v))) <= thr), by = .(election, el, method, rep)]
  d[ok_after == TRUE, .(pct_booths = min(pct_booths),
                        pct_votes = pct_votes[which.min(pct_booths)]),
    by = .(election, el, method, rep)]
}

# ---- Fig 1: assumed reporting order ------------------------------------------
b <- S$booths[election == "2023-07"]
set.seed(cfg$seed + 1)
b[, score := log10(muni_census) + cfg$b_booth * voters / 500 + rnorm(.N, 0, cfg$noise_sd)]
b <- b[order(score)][, pct_booths := 100 * seq_len(.N) / .N]
b[, tier := fcase(muni_census < 5000, T$tiers[1], muni_census < 100000, T$tiers[2],
                  default = T$tiers[3])]
b[, tier := factor(tier, levels = T$tiers[1:3])]
b[, tier_cum := 100 * seq_len(.N) / .N, by = tier]
b[, votes_cum := 100 * cumsum(voters) / sum(voters)]
grid <- seq(0, 100, 0.5)
f1 <- rbind(
  b[, .(pct = grid, y = approx(c(0, pct_booths), c(0, tier_cum), xout = grid, rule = 2)$y),
    by = tier][, series := as.character(tier)],
  b[, .(pct = grid, y = approx(c(0, pct_booths), c(0, votes_cum), xout = grid)$y)][
    , series := T$tiers[4]], fill = TRUE)
f1[, series := factor(series, levels = T$tiers)]
tier_cols <- setNames(c("#9ec5f0", "#4d8fdc", "#1b4f99", "#52514e"), T$tiers)
p1 <- ggplot(f1, aes(pct, y, colour = series, linetype = series == T$tiers[4])) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = tier_cols) +
  scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") +
  scale_x_continuous(labels = pct_lab, expand = expansion(c(0, 0.01))) +
  scale_y_continuous(labels = pct_lab, expand = expansion(c(0, 0.01))) +
  guides(colour = guide_legend(nrow = 2, override.aes = list(linetype = c(1, 1, 1, 2)))) +
  labs(title = T$f1_title, subtitle = T$f1_sub, x = T$x, y = T$f1_y, caption = T$f1_cap) +
  theme_es()
save_png(p1, "fig1_counting_order.png", 9, 6)

# ---- Fig 2: party trajectories, one per election -----------------------------
traj <- merge(res, main, by = c("election", "party"))
traj_b <- traj[, .(med = median(share), lo = quantile(share, 0.1), hi = quantile(share, 0.9)),
               by = .(election, el, party, method, pct_booths, final_share)]
for (e in unique(traj_b$election)) {
  d <- traj_b[election == e]
  # panel titles carry the final result, e.g. "PP  (final: 33.1%)"
  fs <- main[election == e][order(-final_share)]
  plabs <- setNames(sprintf(T$f2_panel, fs$party, pct_lab(fs$final_share, 0.1)), fs$party)
  d[, party := factor(plabs[party], levels = plabs)]
  fl <- unique(d[, .(party, final_share)])
  p <- ggplot(d, aes(pct_booths)) +
    geom_hline(data = fl, aes(yintercept = final_share), colour = ink2,
               linetype = "22", linewidth = 0.5) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = method), alpha = 0.18) +
    geom_line(aes(y = med, colour = method), linewidth = 0.9) +
    facet_wrap(~party, scales = "free_y", nrow = 2) +
    coord_cartesian(xlim = c(0, 100)) +
    sc_method +
    scale_x_continuous(labels = pct_lab, breaks = c(0, 25, 50, 75, 100)) +
    scale_y_continuous(labels = pct_lab) +
    labs(title = sprintf(T$f2_title, T$el_long[e]), subtitle = T$f2_sub,
         x = T$x, y = T$f2_y, caption = caption_txt) +
    theme_es()
  save_png(p, sprintf("fig2_trajectories_%s.png", e), 11, 7)
}

# ---- Fig 3: maximum error in vote share --------------------------------------
eb <- band(err_sum, "max_err")
p3 <- ggplot(eb, aes(pct_booths)) +
  geom_hline(yintercept = 1, colour = ink2, linetype = "22", linewidth = 0.5) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = method), alpha = 0.18) +
  geom_line(aes(y = med, colour = method), linewidth = 0.9) +
  facet_wrap(~el, nrow = 1) +
  coord_cartesian(ylim = c(0, 8)) +
  sc_method +
  scale_x_continuous(labels = pct_lab, breaks = c(0, 50, 100)) +
  scale_y_continuous(labels = label_number(suffix = T$pp)) +
  labs(title = T$f3_title, subtitle = T$f3_sub, x = T$x, y = T$f3_y, caption = caption_txt) +
  theme_es()
save_png(p3, "fig3_vote_share_error.png", 12, 5.5)

# ---- Fig 4: seat error -------------------------------------------------------
sb <- band(seat_err, "seats_off")
p4 <- ggplot(sb, aes(pct_booths)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = method), alpha = 0.18) +
  geom_line(aes(y = med, colour = method), linewidth = 0.9) +
  facet_wrap(~el, nrow = 1) +
  coord_cartesian(ylim = c(0, 40)) +
  sc_method +
  scale_x_continuous(labels = pct_lab, breaks = c(0, 50, 100)) +
  labs(title = T$f4_title, subtitle = T$f4_sub, x = T$x, y = T$f4_y, caption = caption_txt) +
  theme_es()
save_png(p4, "fig4_seat_error.png", 12, 5.5)

# ---- Fig 5: how early can the result be called? ------------------------------
t_share <- time_to(err_sum, "max_err", 1)[, metric := "share"]
t_seats <- time_to(seat_err, "seats_off", 3)[, metric := "seats"]
tt <- rbind(t_share, t_seats)[, .(pct_booths = median(pct_booths), pct_votes = median(pct_votes)),
                              by = .(election, el, method, metric)]
tt <- tt[order(metric, election, method)]
if (lang == "en") {
  fwrite(tt[, .(election, method, metric, pct_booths, pct_votes)], "output/time_to_reliable.csv")
} else {
  # Translated version: ";" separator and decimal comma (opens directly in
  # Excel with Catalan/Spanish regional settings)
  cn <- if (lang == "ca") c("eleccions", "metode", "criteri", "pct_meses_escrutades", "pct_vots_comptats")
        else c("elecciones", "metodo", "criterio", "pct_mesas_escrutadas", "pct_votos_contados")
  out <- tt[, .(T$el_long[election], T$method[method], T$metric[metric],
                round(pct_booths, 1), round(pct_votes, 1))]
  setnames(out, cn)
  fwrite(out, sprintf("output/time_to_reliable_%s.csv", lang), sep = ";", dec = ",", bom = TRUE)
}
tt[, el := factor(el, levels = rev(T$el))]
tt[, metric_lab := factor(T$metric[metric], levels = T$metric)]
seg <- dcast(tt, el + metric_lab ~ method, value.var = "pct_booths")
p5 <- ggplot(tt, aes(pct_booths, el)) +
  geom_segment(data = seg, aes(x = Weighted, xend = `Raw count`, y = el, yend = el),
               colour = "#c3c2b7", linewidth = 1.2) +
  geom_point(aes(colour = method), size = 4) +
  geom_text(aes(label = sprintf(T$f5_lab, pct_lab(pct_booths), pct_lab(pct_votes)), colour = method,
                vjust = ifelse(method == "Weighted", -1.1, 2)),
            size = 3.4, show.legend = FALSE) +
  facet_wrap(~metric_lab, ncol = 1) +
  scale_colour_manual(values = col_method, labels = T$method) +
  scale_x_continuous(limits = c(0, 100), labels = pct_lab) +
  scale_y_discrete(expand = expansion(add = 0.7)) +
  labs(title = T$f5_title, subtitle = T$f5_sub, x = T$x, y = NULL,
       caption = sprintf(T$caption5, cfg$n_reps)) +
  theme_es() + theme(panel.grid.major.y = element_blank())
save_png(p5, "fig5_time_to_call.png", 10, 7)

message("Figures saved in ", out_dir)
