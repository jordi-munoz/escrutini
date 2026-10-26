# =============================================================================
# Simulated election-night count: raw partial results vs results weighted to
# the previous election.
#
# For every election with a previous election available in data/ we:
#   1. Simulate the order in which polling booths (mesas) report, following the
#      usual pattern: small villages first, small/medium towns next, big cities
#      (and the largest booths within them) last.
#   2. At each step of the count (% of booths reported) compute
#        - RAW:      the share each party has among the votes counted so far;
#        - WEIGHTED: the counted booths are reweighted (raking calibration) so
#                    that their PREVIOUS-election results reproduce the previous
#                    results of the whole province. The weighted current votes
#                    are the estimate. Done province by province, then seats are
#                    allocated by D'Hondt (3% threshold) and votes summed
#                    nationally.
#   3. Compare both with the final result (all booths counted; CERA excluded).
#
# Output: output/sim_results.rds
# =============================================================================

source("R/00_functions.R")
suppressPackageStartupMessages(library(parallel))

# ---- Settings ----------------------------------------------------------------
cfg <- list(
  n_reps      = 20,        # simulated counting orders per election
  steps       = c(0.25, 0.5, 1:100),   # % of booths reported at each snapshot
  # Reporting-time model: score = log10(electors of municipality)
  #                               + b_booth * (ballots in booth / 500) + noise
  b_booth     = 0.5,
  noise_sd    = 0.30,
  min_booths  = 30,        # min counted booths to calibrate within a province;
                           # otherwise borrow booths from the region / Spain
  min_share   = 0.03,      # previous-election categories below 3% of census in
                           # a province are merged into "other"
  seed        = 2026
)

elections <- c("02201512_MESA", "02201606_MESA", "02201904_MESA",
               "02201911_MESA", "02202307_MESA")
el_labels <- c("2015-12" = "Dec 2015", "2016-06" = "Jun 2016",
               "2019-04" = "Apr 2019", "2019-11" = "Nov 2019",
               "2023-07" = "Jul 2023")

message("Reading data ...")
E <- lapply(file.path("data", elections), read_election)
names(E) <- sapply(E, function(e) paste0("20", substr(e$tag, 1, 2), "-", substr(e$tag, 3, 4)))

# ---- Build the inputs for one (previous, current) pair ----------------------
prepare_pair <- function(prev, cur) {
  b <- copy(cur$booths)
  N <- nrow(b)

  # Previous election: categories = parties (national code) + other + abstention
  pv <- merge(prev$votes, prev$booths[, .(id, sec_id, muni_id, prov)], by = "id")
  pb <- prev$booths
  valid_nat <- pv[, .(v = sum(votes)), by = party]
  valid_nat[, sh := v / sum(v)]
  prov_sh <- pv[, .(v = sum(votes)), by = .(prov, party)][, sh := v / sum(v), by = prov]
  keep <- union(valid_nat[sh >= 0.01, party], prov_sh[sh >= 0.05, party])
  pv[, cat := ifelse(party %in% keep, paste0("p", party), "other")]

  prof <- function(level) {
    a <- dcast(pv[, .(v = sum(votes)), by = c(level, "cat")],
               as.formula(paste(level, "~ cat")), value.var = "v", fill = 0)
    tot <- pb[, .(census = sum(census), bn = sum(blank + null),
                  voters = sum(blank + null + cand_votes)), by = level]
    a <- merge(tot, a, by = level, all.x = TRUE)
    a[, other := other + bn][, abst := pmax(census - voters, 0)]
    cats <- c(paste0("p", keep), "other", "abst")
    for (cc in setdiff(cats, names(a))) a[, (cc) := 0]
    m <- as.matrix(a[, ..cats]); m[is.na(m)] <- 0
    m <- m / pmax(rowSums(m), 1)                       # shares of census
    list(key = a[[level]], m = m, cats = cats)
  }
  ps <- prof("sec_id"); pm <- prof("muni_id"); pp <- prof("prov")

  # Match each current booth to its previous section; else municipality; else province
  i_s <- match(b$sec_id, ps$key); i_m <- match(b$muni_id, pm$key); i_p <- match(b$prov, pp$key)
  S <- matrix(NA_real_, N, length(ps$cats), dimnames = list(NULL, ps$cats))
  use_s <- !is.na(i_s); use_m <- is.na(i_s) & !is.na(i_m); use_p <- is.na(i_s) & is.na(i_m)
  S[use_s, ] <- ps$m[i_s[use_s], ]
  S[use_m, ] <- pm$m[i_m[use_m], ]
  S[use_p, ] <- pp$m[i_p[use_p], ]
  X <- S * b$census                                    # expected previous counts
  match_info <- c(section = mean(use_s), municipality = mean(use_m), province = mean(use_p))

  # Current votes: booth x party (+ blank)
  cv <- dcast(cur$votes, id ~ party, value.var = "votes", fill = 0)
  cv <- cv[match(b$id, cv$id)]
  Y <- as.matrix(cv[, -"id"]); Y[is.na(Y)] <- 0
  colnames(Y) <- names(cv)[-1]
  Y <- cbind(Y, blank = b$blank)

  provs <- sort(unique(b$prov))
  prov_idx <- lapply(provs, function(p) which(b$prov == p))
  prov_ccaa <- b[, .(ccaa = ccaa[1]), by = prov][match(provs, prov)]$ccaa
  prov_cols <- lapply(prov_idx, function(ix) which(colSums(Y[ix, , drop = FALSE]) > 0))
  Tgt <- t(sapply(prov_idx, function(ix) colSums(X[ix, , drop = FALSE])))

  list(b = b, X = X, Y = Y, provs = provs, prov_idx = prov_idx,
       prov_ccaa = prov_ccaa, prov_cols = prov_cols, Tgt = Tgt,
       seats = cur$seats[match(provs, prov)]$seats,
       parties = cur$parties, match_info = match_info)
}

col_labels <- function(P) {
  pc <- setdiff(colnames(P$Y), "blank")
  setNames(party_label(P$parties$siglas[match(as.numeric(pc), P$parties$party)]), pc)
}

# ---- Estimates for one snapshot ---------------------------------------------
estimate_snapshot <- function(P, counted) {
  np <- length(P$provs); J <- ncol(P$Y)
  raw <- wtd <- matrix(0, np, J, dimnames = list(P$provs, colnames(P$Y)))
  pool_level <- integer(np)
  cnt_prov <- P$b$prov[counted]; cnt_ccaa <- P$b$ccaa[counted]
  for (k in seq_len(np)) {
    cols <- P$prov_cols[[k]]
    in_p <- counted[cnt_prov == P$provs[k]]
    if (length(in_p)) raw[k, cols] <- colSums(P$Y[in_p, cols, drop = FALSE])

    # calibration targets for this province: small categories merged
    tg <- P$Tgt[k, ]
    big <- names(tg)[tg / sum(tg) >= cfg$min_share & names(tg) != "other"]
    small <- setdiff(names(tg), big)
    pools <- list(in_p, counted[cnt_ccaa == P$prov_ccaa[k]], counted)
    for (lev in 1:3) {
      ix <- pools[[lev]]
      if (length(ix) < cfg$min_booths && lev < 3) next
      Xk <- cbind(P$X[ix, big, drop = FALSE],
                  other = rowSums(P$X[ix, small, drop = FALSE]))
      Tk <- c(tg[big], other = sum(tg[small]))
      cal <- calibrate_raking(Xk, Tk)
      if (cal$ok || lev == 3) {
        wtd[k, cols] <- colSums(cal$w * P$Y[ix, cols, drop = FALSE])
        pool_level[k] <- lev
        break
      }
    }
  }
  list(raw = raw, wtd = wtd, pool_level = pool_level)
}

# Aggregate a province x party matrix into national votes and seats by label
summarise_est <- function(M, P, lab, fill_from = NULL) {
  # provinces with no counted votes (raw method only): use national shares
  if (!is.null(fill_from)) {
    empty <- rowSums(M) == 0
    if (any(empty)) {
      nat <- colSums(M); nat <- nat / sum(nat)
      M[empty, ] <- matrix(nat, sum(empty), ncol(M), byrow = TRUE)
    }
  }
  pcols <- setdiff(colnames(M), "blank")
  seats <- t(sapply(seq_len(nrow(M)), function(k)
    dhondt(M[k, pcols], sum(M[k, ]), P$seats[k])))
  colnames(seats) <- pcols
  valid <- sum(M)
  v <- tapply(colSums(M[, pcols, drop = FALSE]), lab[pcols], sum)
  s <- tapply(colSums(seats), lab[pcols], sum)
  data.table(party = names(v), share = 100 * as.numeric(v) / valid,
             seats = as.numeric(s[names(v)]))
}

simulate_pair <- function(P, rep) {
  b <- P$b; N <- nrow(b)
  set.seed(cfg$seed + rep)
  score <- log10(b$muni_census) + cfg$b_booth * b$voters / 500 +
           rnorm(N, 0, cfg$noise_sd)
  ord <- order(score)
  lab <- col_labels(P)
  tot_votes <- sum(b$voters)
  out <- vector("list", length(cfg$steps))
  for (s in seq_along(cfg$steps)) {
    n_c <- max(1, ceiling(cfg$steps[s] / 100 * N))
    counted <- sort(ord[seq_len(n_c)])
    est <- estimate_snapshot(P, counted)
    r <- summarise_est(est$raw, P, lab, fill_from = TRUE)[, method := "Raw count"]
    w <- summarise_est(est$wtd, P, lab)[, method := "Weighted"]
    out[[s]] <- rbind(r, w)[, `:=`(rep = rep, pct_booths = cfg$steps[s],
                                   pct_votes = 100 * sum(b$voters[counted]) / tot_votes,
                                   prov_own_pool = mean(est$pool_level == 1))]
  }
  rbindlist(out)
}

# ---- Run ---------------------------------------------------------------------
run_simulation <- function() {
  pairs <- data.table(prev = names(E)[-length(E)], cur = names(E)[-1])
  ncores <- max(1, min(cfg$n_reps, detectCores() - 1))
  results <- list(); finals <- list(); info <- list(); booths <- list()

  for (i in seq_len(nrow(pairs))) {
    cur <- pairs$cur[i]; prev <- pairs$prev[i]
    message("Simulating ", cur, " (weights from ", prev, ") ...")
    t0 <- Sys.time()
    P <- prepare_pair(E[[prev]], E[[cur]])
    lab <- col_labels(P)
    M_final <- t(sapply(P$prov_idx, function(ix) colSums(P$Y[ix, , drop = FALSE])))
    finals[[cur]] <- summarise_est(M_final, P, lab)[, election := cur]
    info[[cur]] <- data.table(election = cur, prev = prev, t(P$match_info))
    booths[[cur]] <- P$b[, .(election = cur, muni_census, voters, census)]

    cl <- makeCluster(ncores)
    clusterExport(cl, c("cfg", "P", "estimate_snapshot", "summarise_est", "simulate_pair",
                        "calibrate_raking", "dhondt", "party_label", "col_labels"),
                envir = environment())
    invisible(clusterEvalQ(cl, library(data.table)))
    res <- parLapply(cl, seq_len(cfg$n_reps), function(r) simulate_pair(P, r))
    stopCluster(cl)
    results[[cur]] <- rbindlist(res)[, election := cur]
    message("  done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
  }

  dir.create("output", showWarnings = FALSE)
  saveRDS(list(results = rbindlist(results), finals = rbindlist(finals),
               match_info = rbindlist(info), booths = rbindlist(booths),
               cfg = cfg, el_labels = el_labels),
          "output/sim_results.rds")
  message("Saved output/sim_results.rds")
}

# Set options(escrutini.norun = TRUE) before sourcing to load functions only
if (!isTRUE(getOption("escrutini.norun"))) run_simulation()
