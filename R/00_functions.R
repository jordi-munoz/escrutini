# =============================================================================
# Helper functions: reading Ministry of Interior files, calibration weighting,
# D'Hondt seat allocation.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# ---- Reading fixed-width files (Ministerio del Interior, "MESA" packages) ----

read_fw <- function(path) {
  x <- readLines(path, encoding = "latin1", warn = FALSE)
  x <- iconv(x, from = "latin1", to = "UTF-8")
  x[nchar(x) > 0]
}
num <- function(x, a, b) as.numeric(substr(x, a, b))
chr <- function(x, a, b) trimws(substr(x, a, b))

# Reads one election folder (e.g. data/02202307_MESA) and returns a list with
#   booths: one row per polling booth (mesa), CERA excluded
#   votes : long table booth x party (party = national accumulation code)
#   parties: code -> label
#   seats : seats per province
read_election <- function(dir) {
  tag <- sub("^02\\d{2}(\\d{4})_MESA$", "\\1", basename(dir))   # yymm, e.g. 2307
  f <- function(n) file.path(dir, sprintf("%02d02%s.DAT", n, tag))

  # 03: candidaturas
  x <- read_fw(f(3))
  cand <- data.table(code = num(x, 9, 14), siglas = chr(x, 15, 64),
                     name = chr(x, 65, 214), nat = num(x, 227, 232))
  cand[is.na(nat) | nat == 0, nat := code]
  parties <- cand[code == nat, .(party = nat, siglas, name)]
  parties <- unique(rbind(parties,
                          cand[!nat %in% parties$party, .(party = nat, siglas, name)]),
                    by = "party")

  # 07: seats per province (district digit 9 = provincial total)
  x <- read_fw(f(7))
  seats <- data.table(ccaa = num(x, 10, 11), prov = num(x, 12, 13),
                      dist = num(x, 14, 14), seats = num(x, 150, 155))
  seats <- seats[prov != 99 & dist == 9, .(prov, seats)]

  # 09: booths
  x <- read_fw(f(9))
  booths <- data.table(
    ccaa = num(x, 10, 11), prov = num(x, 12, 13), muni = num(x, 14, 16),
    dist = num(x, 17, 18), secc = chr(x, 19, 22), mesa = chr(x, 23, 23),
    census = num(x, 31, 37), blank = num(x, 66, 72), null = num(x, 73, 79),
    cand_votes = num(x, 80, 86))
  booths <- booths[muni != 999]                      # drop CERA (votes from abroad)
  booths[, id := sprintf("%02d%03d%02d%s%s", prov, muni, dist, secc, mesa)]
  booths[, sec_id := sprintf("%02d%03d%02d%s", prov, muni, dist, secc)]
  booths[, muni_id := sprintf("%02d%03d", prov, muni)]
  booths[, voters := blank + null + cand_votes]
  booths[, muni_census := sum(census), by = muni_id]

  # 10: votes per booth and candidacy
  x <- read_fw(f(10))
  votes <- data.table(prov = num(x, 12, 13), muni = num(x, 14, 16),
                      dist = num(x, 17, 18), secc = chr(x, 19, 22),
                      mesa = chr(x, 23, 23), code = num(x, 24, 29),
                      votes = num(x, 30, 36))
  votes <- votes[muni != 999]
  votes[, id := sprintf("%02d%03d%02d%s%s", prov, muni, dist, secc, mesa)]
  votes <- merge(votes[, .(id, code, votes)], cand[, .(code, nat)], by = "code")
  votes <- votes[, .(votes = sum(votes)), by = .(id, party = nat)]

  # In some files (notably 2015) the booth total of candidacy votes does not
  # match the sum of party votes; party votes are taken as the reference.
  booths[votes[, .(v = sum(votes)), by = id], cand_votes := i.v, on = "id"]
  booths[, voters := blank + null + cand_votes]

  list(tag = tag, booths = booths, votes = votes, parties = parties, seats = seats)
}

# ---- Short, harmonised party labels ----------------------------------------
party_label <- function(siglas) {
  s <- toupper(siglas)
  fcase(
    grepl("^PP$|^PP-|PARTIDO POPULAR", s), "PP",
    grepl("^PSOE|^PSC", s), "PSOE",
    grepl("^VOX", s), "Vox",
    grepl("^SUMAR", s), "Sumar",
    grepl("PODEMOS|UNIDAS|UNIDOS|^UP$|EN COM|^ECP", s), "Podemos/UP",
    grepl("^C'S$|^CS$|CIUDADANOS|^C.S$", s), "Cs",
    grepl("^ERC|ESQUERRA", s), "ERC",
    grepl("JUNTS|^JXCAT|^DL$|DEMOCRÀCIA I LLIBERTAT", s), "Junts/DL",
    grepl("^EAJ|^PNV", s), "PNV",
    grepl("BILDU", s), "EH Bildu",
    default = siglas)
}

# ---- Calibration weighting ---------------------------------------------------
# Finds weights w_i = d * exp(z_i' lambda) for the counted booths such that the
# weighted previous-election totals of the counted booths reproduce the
# previous-election totals of the whole constituency:  sum_i w_i x_i = Tgt.
#   X   : n x k matrix of previous-election counts (parties, other, abstention)
#   Tgt : length-k target totals
# z_i are the row shares of X (instrumental raking), which keeps the problem
# well scaled. Returns list(w, ok).
calibrate_raking <- function(X, Tgt, maxit = 60, tol = 1e-4) {
  n <- nrow(X); k <- ncol(X)
  rs <- rowSums(X); rs[rs == 0] <- 1
  Z <- X / rs
  d <- sum(Tgt) / sum(X)
  lambda <- numeric(k)
  w <- rep(d, n)
  res <- colSums(w * X) - Tgt
  nres <- sqrt(sum((res / Tgt)^2))
  for (it in seq_len(maxit)) {
    if (nres < tol) break
    J <- crossprod(X, w * Z)                         # k x k Jacobian
    step <- tryCatch(solve(J, -res), error = function(e) NULL)
    if (is.null(step)) break
    a <- 1
    repeat {                                          # damped Newton step
      lam_new <- lambda + a * step
      eta <- drop(Z %*% lam_new)
      if (max(eta) > 30) { a <- a / 2; if (a < 1e-6) break; next }
      w_new <- d * exp(eta)
      res_new <- colSums(w_new * X) - Tgt
      nres_new <- sqrt(sum((res_new / Tgt)^2))
      if (nres_new < nres || a < 1e-6) break
      a <- a / 2
    }
    if (a < 1e-6) break
    lambda <- lam_new; w <- w_new; res <- res_new; nres <- nres_new
  }
  list(w = w, ok = nres < 1e-2)
}

# ---- D'Hondt with 3% provincial threshold ------------------------------------
# v: named vector of party votes in one province; valid: valid votes (parties +
# blank); S: number of seats. Returns named vector of seats.
dhondt <- function(v, valid, S) {
  out <- setNames(numeric(length(v)), names(v))
  ok <- which(v >= 0.03 * valid & v > 0)
  if (!length(ok) || S == 0) return(out)
  q <- outer(v[ok], seq_len(S), "/")
  top <- order(q, decreasing = TRUE)[seq_len(S)]
  idx <- ((top - 1) %% length(ok)) + 1
  tab <- tabulate(idx, nbins = length(ok))
  out[ok] <- tab
  out
}
