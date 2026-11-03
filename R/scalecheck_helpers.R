# =====================================================
# ScaleCheck — shared helpers
# -----------------------------------------------------
# Used by both app.R (Shiny autoloads the R/ folder)
# and Report.Rmd (sourced at render time), so the app
# and the downloadable report always compute, interpret
# and print R code in exactly the same way.
# =====================================================

# =====================================================
# GENERAL UTILITIES
# =====================================================

sc_data_type <- function(dat) {
  ncat <- vapply(dat, function(x) length(unique(stats::na.omit(x))), numeric(1))
  if (all(ncat == 2)) {
    "binary"
  } else if (all(ncat > 2 & ncat <= 7)) {
    "polytomous"
  } else if (all(ncat >= 2 & ncat <= 7)) {
    "mixed"
  } else {
    "continuous"
  }
}

sc_type_label <- function(type) {
  switch(
    type,
    binary     = "Dichotomous (2 categories)",
    polytomous = "Polytomous (3–7 ordered categories)",
    mixed      = "Mixed dichotomous / polytomous",
    continuous = "Continuous or > 7 categories"
  )
}

sc_fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

sc_item_list <- function(x) {
  if (length(x) == 0) return("none")
  paste0("<b>", x, "</b>", collapse = ", ")
}

# Callout box: level is "good", "warn", "bad" or "info"
sc_callout <- function(level, title, body) {
  icon <- switch(level, good = "&#10004;", warn = "&#9888;", bad = "&#10006;", "&#9432;")
  sprintf(
    "<div class='sc-callout sc-%s'><div class='sc-callout-title'>%s %s</div>%s</div>",
    level, icon, title, body
  )
}

sc_bullets <- function(x) {
  x <- x[nzchar(x)]
  if (length(x) == 0) return("")
  paste0("<ul>", paste0("<li>", x, "</li>", collapse = ""), "</ul>")
}

sc_error_html <- function(what, e) {
  sc_callout(
    "bad",
    paste(what, "could not be estimated"),
    paste0("<p>", htmltools::htmlEscape(conditionMessage(e)), "</p>",
           "<p>This usually means there is too little variability, too few items, ",
           "too much missing data, or a response format that does not suit this method.</p>")
  )
}

sc_pairs <- function(mat, keep) {
  idx <- which(keep & upper.tri(mat), arr.ind = TRUE)
  if (nrow(idx) == 0) return(data.frame(Item_1 = character(), Item_2 = character(), r = numeric()))
  data.frame(
    Item_1 = rownames(mat)[idx[, 1]],
    Item_2 = colnames(mat)[idx[, 2]],
    r      = mat[idx],
    stringsAsFactors = FALSE
  )
}

# HTML table with flagged rows highlighted (used in the app and the report)
sc_table_html <- function(df, digits = 3, flag_col = "Flag", caption = NULL) {
  if (is.null(df) || !nrow(df)) return("<p class='text-muted'><i>No rows to display.</i></p>")
  fmt <- lapply(df, function(x) {
    if (is.numeric(x)) ifelse(is.na(x), "", formatC(x, format = "f", digits = if (all(x == round(x), na.rm = TRUE)) 0 else digits))
    else ifelse(is.na(x), "", htmltools::htmlEscape(as.character(x)))
  })
  flagged <- if (flag_col %in% names(df)) !is.na(df[[flag_col]]) & nzchar(df[[flag_col]]) else rep(FALSE, nrow(df))
  head <- paste0("<th>", htmltools::htmlEscape(gsub("_", " ", names(df))), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- paste0("<td>", vapply(fmt, `[`, character(1), i), "</td>", collapse = "")
    sprintf("<tr%s>%s</tr>", if (flagged[i]) " class='sc-flag'" else "", cells)
  }, character(1))
  paste0(
    "<div class='table-responsive'><table class='table table-sm table-hover sc-table'>",
    if (!is.null(caption)) paste0("<caption>", caption, "</caption>") else "",
    "<thead><tr>", head, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"
  )
}

reliability_label <- function(x) {
  if (is.na(x)) "Not estimated"
  else if (x >= .90) "Excellent"
  else if (x >= .80) "Good"
  else if (x >= .70) "Acceptable"
  else if (x >= .60) "Questionable"
  else "Poor"
}

reliability_level <- function(x) {
  if (is.na(x)) "info" else if (x >= .80) "good" else if (x >= .70) "warn" else "bad"
}

# =====================================================
# EXAMPLE DATA
# =====================================================

sc_example_data <- function(name) {
  switch(
    name,
    "Big Five Inventory (polytomous data)" = psych::bfi[, c(6:10, 1:5, 11:25)],
    "Simulated Unidimensional (dichotomous data)" = {
      if (requireNamespace("birm", quietly = TRUE)) {
        simdata <- birm::simData(n = 1000, v = 10, l = 2, model = "rasch", seed = 666)
        as.data.frame(simdata$data)
      } else {
        set.seed(666)
        sim <- as.data.frame(eRm::sim.rasch(1000, 10))
        names(sim) <- paste0("V", 1:10)
        sim
      }
    }
  )
}

# =====================================================
# ANALYSES
# =====================================================

# -----------------------------------------------------
# Descriptives
# -----------------------------------------------------

sc_descriptives <- function(dat) {
  d <- as.data.frame(psych::describe(dat))
  tab <- data.frame(
    Item     = rownames(d),
    n        = d$n,
    Mean     = d$mean,
    SD       = d$sd,
    Median   = d$median,
    Min      = d$min,
    Max      = d$max,
    Skew     = d$skew,
    Kurtosis = d$kurtosis,
    Missing_pct = 100 * colMeans(is.na(dat)),
    stringsAsFactors = FALSE
  )

  freq <- tryCatch({
    # psych >= 2.5 renamed response.frequencies() to responseFrequency()
    rf <- if (exists("responseFrequency", envir = asNamespace("psych"))) {
      getExportedValue("psych", "responseFrequency")
    } else {
      getExportedValue("psych", "response.frequencies")
    }
    f <- rf(dat, max = 10)
    f <- as.data.frame(round(100 * f, 1))
    names(f) <- ifelse(names(f) == "miss", "Missing %", paste0("Cat ", names(f), " %"))
    data.frame(Item = rownames(f), f, check.names = FALSE, row.names = NULL)
  }, error = function(e) NULL)

  # sparse categories (< 2% of valid responses) only make sense for ordinal items
  sparse <- vapply(dat, function(x) {
    p <- prop.table(table(x))
    length(p) <= 10 && any(p < .02)
  }, logical(1))

  list(
    table    = tab,
    freq     = freq,
    n        = nrow(dat),
    k        = ncol(dat),
    complete = sum(stats::complete.cases(dat)),
    type     = sc_data_type(dat),
    skewed   = tab$Item[abs(tab$Skew) > 2],
    kurtotic = tab$Item[abs(tab$Kurtosis) > 7],
    missing  = tab$Item[tab$Missing_pct > 10],
    sparse   = names(sparse)[sparse],
    constant = tab$Item[tab$SD == 0 | is.na(tab$SD)]
  )
}

sc_interp_desc <- function(res) {
  problems <- c(
    if (length(res$constant)) paste("Items with no variance (remove before analysis):", sc_item_list(res$constant)),
    if (length(res$missing))  paste("Items with more than 10% missing responses:", sc_item_list(res$missing)),
    if (length(res$skewed))   paste("Strongly skewed items (|skew| > 2):", sc_item_list(res$skewed)),
    if (length(res$kurtotic)) paste("Items with extreme kurtosis (|kurtosis| > 7):", sc_item_list(res$kurtotic)),
    if (length(res$sparse))   paste("Items with sparse response categories (< 2% of responses):", sc_item_list(res$sparse),
                                    "— consider collapsing adjacent categories before IRT/CFA.")
  )
  body <- paste0(
    sprintf("<p>The selection contains <b>%d respondents</b> and <b>%d items</b>; <b>%d respondents (%.1f%%)</b> have complete data. Response format detected: <b>%s</b>.</p>",
            res$n, res$k, res$complete, 100 * res$complete / res$n, sc_type_label(res$type)),
    if (length(problems)) sc_bullets(problems) else "<p>No item shows extreme skewness, kurtosis, missingness or sparse categories.</p>",
    if (res$complete < res$n) "<p>Several methods (Parallel Analysis, Hull, Mokken, T1/T11) use <b>listwise deletion</b>, so they are run on the complete cases only.</p>" else ""
  )
  level <- if (length(res$constant)) "bad" else if (length(problems)) "warn" else "good"
  sc_callout(level, "Data screening", body)
}

# -----------------------------------------------------
# Correlations
# -----------------------------------------------------

sc_correlations <- function(dat, method = "spearman") {
  ct <- psych::corr.test(dat, method = method, use = "pairwise", adjust = "holm")
  r <- ct$r
  ut <- r[upper.tri(r)]
  list(
    r       = r,
    p       = ct$p,
    method  = method,
    mean_r  = mean(ut, na.rm = TRUE),
    min_r   = min(ut, na.rm = TRUE),
    max_r   = max(ut, na.rm = TRUE),
    neg     = sc_pairs(r, r < 0),
    high    = sc_pairs(r, abs(r) >= .80),
    weak    = names(which(apply(abs(r) - diag(ncol(r)), 2, max, na.rm = TRUE) < .20))
  )
}

sc_plot_cor <- function(res) {
  corrplot::corrplot(
    res$r,
    method = "pie",
    type = "upper",
    diag = FALSE,
    number.cex = 0.8,
    p.mat = res$p,
    tl.col = "black",
    addCoef.col = "black",
    tl.srt = 45,
    tl.cex = 0.9,
    col = grDevices::colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
    na.label = " "
  )
}

sc_interp_cor <- function(res) {
  items_neg <- unique(c(res$neg$Item_1, res$neg$Item_2))
  bullets <- c(
    sprintf("Average inter-item correlation: <b>%.2f</b> (range %.2f to %.2f). Values between .15 and .50 are typical for a coherent but not redundant scale.",
            res$mean_r, res$min_r, res$max_r),
    if (nrow(res$neg)) sprintf("<b>%d negative correlation(s)</b> involving %s. Check whether these items are reverse-keyed and need recoding before computing scores or reliability.",
                               nrow(res$neg), sc_item_list(items_neg)),
    if (nrow(res$high)) sprintf("<b>Very high correlations (|r| &ge; .80)</b>: %s — possible item redundancy or local dependence.",
                                paste0(res$high$Item_1, "&ndash;", res$high$Item_2, collapse = ", ")),
    if (length(res$weak)) sprintf("Items that do not correlate above .20 with any other item: %s.", sc_item_list(res$weak))
  )
  level <- if (nrow(res$neg) || nrow(res$high)) "warn" else if (res$mean_r < .15) "warn" else "good"
  sc_callout(level, paste0("Correlation structure (", res$method, ")"), sc_bullets(bullets))
}

# -----------------------------------------------------
# Reliability
# -----------------------------------------------------

sc_reliability <- function(dat, check_keys = FALSE) {
  a <- suppressWarnings(psych::alpha(dat, check.keys = check_keys, warnings = FALSE))

  om <- tryCatch(
    suppressWarnings(suppressMessages(psych::omega(dat, plot = FALSE))),
    error = function(e) tryCatch(
      suppressWarnings(suppressMessages(psych::omega(dat, nfactors = 1, plot = FALSE))),
      error = function(e) NULL
    )
  )

  glb <- tryCatch(suppressWarnings(psych::glb(dat)$glb.Fa), error = function(e) NA)

  ci <- tryCatch(
    c(a$feldt$lower.ci$raw_alpha, a$feldt$upper.ci$raw_alpha),
    error = function(e) c(NA, NA)
  )

  items <- data.frame(
    Item           = rownames(a$item.stats),
    Item_rest_r    = a$item.stats$r.drop,
    Alpha_if_drop  = a$alpha.drop$raw_alpha,
    stringsAsFactors = FALSE
  )
  alpha <- a$total$raw_alpha
  items$Flag <- ifelse(items$Item_rest_r < 0, "Negative item-rest r",
                ifelse(items$Item_rest_r < .30, "Weak item-rest r",
                ifelse(items$Alpha_if_drop > alpha + .01, "Alpha increases if dropped", "")))

  list(
    alpha    = alpha,
    alpha_ci = ci,
    omega    = if (is.null(om)) NA else om$omega.tot,
    omega_h  = if (is.null(om)) NA else om$omega_h,
    glb      = if (length(glb)) as.numeric(glb[1]) else NA,
    items    = items,
    reversed = sub("-$", "", grep("-$", items$Item, value = TRUE)),
    check_keys = check_keys
  )
}

sc_interp_rel <- function(res) {
  weak <- res$items$Item[res$items$Flag != ""]
  bullets <- c(
    sprintf("Cronbach's &alpha; = <b>%.2f</b> (95%% CI %.2f–%.2f) → <b>%s</b>.",
            res$alpha, res$alpha_ci[1], res$alpha_ci[2], reliability_label(res$alpha)),
    sprintf("McDonald's &omega; total = <b>%s</b> → <b>%s</b>. &omega; hierarchical = <b>%s</b> (share of variance due to a single general factor; values &ge; .70 support interpreting a total score).",
            sc_fmt(res$omega), reliability_label(res$omega), sc_fmt(res$omega_h)),
    sprintf("GLB = <b>%s</b>. The GLB tends to overestimate reliability in small samples; read it as an upper reference.", sc_fmt(res$glb)),
    if (any(res$items$Item_rest_r < 0) && !res$check_keys)
      "<b>Some items correlate negatively with the rest of the scale</b>, which pushes alpha down (even below zero). They are probably reverse-keyed: recode them, or tick <i>check.keys</i> to reverse them automatically. &omega; already reverses them internally, which explains why it is much higher than &alpha;."
    else if (!is.na(res$omega) && res$omega - res$alpha > .05)
      "Omega is noticeably higher than alpha, which suggests factor loadings differ across items (tau-equivalence is violated): prefer &omega;.",
    if (res$alpha >= .95) "Very high alpha may indicate redundant items (near-duplicate wording) rather than better measurement.",
    if (length(weak)) paste("Items worth reviewing (weak or negative item-rest correlation, or alpha rises when removed):", sc_item_list(weak)),
    if (length(res$reversed)) paste("Items automatically reverse-keyed for alpha:", sc_item_list(res$reversed), "— recode them in your data for the other analyses.")
  )
  main <- if (!is.na(res$omega)) res$omega else res$alpha
  sc_callout(reliability_level(main), "Reliability", sc_bullets(bullets))
}

# -----------------------------------------------------
# Parallel Analysis
# -----------------------------------------------------

sc_parallel <- function(dat, n_datasets = 20, percent = 95, corr = "Polychoric") {
  X <- as.matrix(stats::na.omit(dat))
  txt <- utils::capture.output(
    pa <- EFA.MRFA::parallelMRFA(
      X,
      Ndatsets = n_datasets,
      percent = percent,
      corr = corr,
      display = TRUE,
      graph = FALSE
    )
  )
  list(pa = pa, text = txt, n_used = nrow(X), n_datasets = n_datasets, percent = percent, corr = corr)
}

sc_plot_pa <- function(res) {
  pa <- res$pa
  k <- length(pa$Real_Data)
  x <- seq_len(k)
  yl <- range(c(pa$Real_Data, pa$Percentile_random, pa$Mean_random), na.rm = TRUE)
  op <- graphics::par(mar = c(4.5, 4.5, 2.5, 1))
  on.exit(graphics::par(op))
  graphics::plot(x, pa$Real_Data, type = "b", pch = 19, lwd = 2, col = "#2f5d8a", ylim = yl,
                 xlab = "Factor", ylab = "% of explained common variance",
                 main = "Parallel Analysis (MRFA)", xaxt = "n", las = 1)
  graphics::axis(1, at = x)
  graphics::lines(x, pa$Percentile_random, type = "b", pch = 17, lwd = 2, lty = 2, col = "#c0392b")
  graphics::lines(x, pa$Mean_random, type = "b", pch = 15, lwd = 1.5, lty = 3, col = "#7f8c8d")
  graphics::abline(v = pa$N_factors_percentiles + 0.5, col = "#e6b800", lwd = 2, lty = 4)
  graphics::legend("topright", bty = "n",
                   legend = c("Real data", paste0(res$percent, "th percentile of random data"), "Mean of random data"),
                   col = c("#2f5d8a", "#c0392b", "#7f8c8d"), pch = c(19, 17, 15), lty = c(1, 2, 3), lwd = 2)
}

sc_interp_pa <- function(res) {
  nf <- res$pa$N_factors_percentiles
  nm <- res$pa$N_factors_mean
  body <- paste0(
    sprintf("<p>Using the <b>%dth percentile</b> criterion (%d random datasets, %s correlations, %d complete cases), Parallel Analysis suggests retaining <b>%d factor%s</b>. With the more liberal mean criterion it suggests <b>%d</b>.</p>",
            res$percent, res$n_datasets, res$corr, res$n_used, nf, if (nf == 1) "" else "s", nm),
    if (nf == 1) "<p>Only the first factor explains more common variance than expected by chance, which supports a <b>unidimensional</b> scale.</p>"
    else sprintf("<p>%d factors explain more common variance than random data. Inspect which items cluster together (see EGA) before treating the scale as a single score.</p>", nf),
    if (res$n_datasets < 100) "<p><i>Tip:</i> fewer than 100 random datasets gives an unstable percentile. Increase the number for final results.</p>" else ""
  )
  sc_callout(if (nf == 1) "good" else "warn", "Parallel Analysis", body)
}

# -----------------------------------------------------
# EGA
# -----------------------------------------------------

sc_ega <- function(dat, model = "glasso", algorithm = "walktrap") {
  e <- suppressMessages(EGAnet::EGA(dat, model = model, algorithm = algorithm, plot.EGA = FALSE))
  loads <- tryCatch(suppressMessages(EGAnet::net.loads(e)$std), error = function(err) NULL)
  membership <- data.frame(Item = names(e$wc), Dimension = as.vector(e$wc), stringsAsFactors = FALSE)
  if (!is.null(loads)) {
    loads <- as.data.frame(round(loads, 3))
    loads$Item <- rownames(loads)
    membership <- merge(membership, loads, by = "Item", sort = FALSE)
  }
  list(
    ega        = e,
    n_dim      = e$n.dim,
    membership = membership,
    text       = utils::capture.output(print(e)),
    model      = model,
    algorithm  = algorithm,
    tefi       = tryCatch(as.numeric(e$TEFI), error = function(err) NA)
  )
}

sc_interp_ega <- function(res) {
  unassigned <- res$membership$Item[is.na(res$membership$Dimension)]
  groups <- split(res$membership$Item, res$membership$Dimension)
  group_txt <- vapply(names(groups), function(g) sprintf("Dimension %s: %s", g, sc_item_list(groups[[g]])), character(1))
  body <- paste0(
    sprintf("<p>EGA (%s network, %s community detection) found <b>%s dimension%s</b>.</p>",
            res$model, res$algorithm, res$n_dim, if (isTRUE(res$n_dim == 1)) "" else "s"),
    sc_bullets(c(
      group_txt,
      if (length(unassigned)) paste("Items not assigned to any community (weakly connected):", sc_item_list(unassigned))
    )),
    if (isTRUE(res$n_dim == 1)) "<p>A single community supports a unidimensional structure.</p>"
    else "<p>Several communities suggest distinct but possibly related sub-dimensions. Check whether they match the content of the items (e.g. subscales) and compare with Parallel Analysis and the Hull method.</p>",
    "<p>Network loadings (in the table) show how strongly each item connects to its community. Standardized values above about .15 are small, above .25 moderate and above .35 large (Christensen &amp; Golino, 2021).</p>"
  )
  sc_callout(if (isTRUE(res$n_dim == 1) && !length(unassigned)) "good" else "warn", "Exploratory Graph Analysis", body)
}

# -----------------------------------------------------
# Hull
# -----------------------------------------------------

sc_hull <- function(dat, extr = "ULS", index = "CAF") {
  X <- stats::na.omit(dat)
  # hullEFA() returns NULL when display = TRUE, so the printout is rebuilt from its matrix
  utils::capture.output(
    h <- EFA.MRFA::hullEFA(X, extr = extr, index_hull = index, display = FALSE, graph = FALSE)
  )
  txt <- c(
    paste("HULL METHOD -", index, "INDEX"), "",
    utils::capture.output(print(round(as.data.frame(h$Matrix), 4), row.names = FALSE)), "",
    paste("Number of advised dimensions:", h$n_factors)
  )
  list(hull = h, text = txt, n_factors = h$n_factors, extr = extr, index = index, n_used = nrow(X))
}

sc_plot_hull <- function(res) {
  m <- as.data.frame(res$hull$Matrix)
  op <- graphics::par(mar = c(4.5, 4.5, 2.5, 1))
  on.exit(graphics::par(op))
  graphics::plot(m$g, m$f, type = "b", pch = 19, lwd = 2, col = "#2f5d8a", las = 1,
                 xlab = "Degrees of freedom (model complexity)", ylab = paste("Goodness of fit:", res$index),
                 main = "Hull Method")
  graphics::text(m$g, m$f, labels = paste0(m$q, "F"), pos = 3, cex = 0.85)
  sel <- m$q == res$n_factors
  graphics::points(m$g[sel], m$f[sel], pch = 21, cex = 2.6, lwd = 2.5, col = "#e6b800")
}

sc_interp_hull <- function(res) {
  nf <- res$n_factors
  body <- paste0(
    sprintf("<p>Using %s extraction and the %s fit index, the Hull method suggests <b>%d factor%s</b> as the best balance between fit and parsimony (%d complete cases).</p>",
            res$extr, res$index, nf, if (nf == 1) "" else "s", res$n_used),
    if (nf == 1) sc_bullets(c(
      "A single dominant dimension explains the covariance structure well enough.",
      "Extra factors would improve fit only marginally and probably reflect minor residual structure.",
      "The scale can be treated as essentially unidimensional in later modeling."
    )) else sc_bullets(c(
      sprintf("The data probably contain %d meaningful latent dimensions.", nf),
      "Items may cluster into sub-domains. Use EGA membership to see which ones.",
      "A one-factor CFA or a unidimensional Rasch model will probably show misfit."
    )),
    if (nf == 0) "<p>A solution of 0 factors means the items share very little common variance.</p>" else ""
  )
  sc_callout(if (nf == 1) "good" else "warn", "Hull Method", body)
}

# -----------------------------------------------------
# CFA
# -----------------------------------------------------

sc_cfa_syntax <- function(vars) {
  paste0("F1 =~ ", paste(vars, collapse = " + "))
}

sc_cfa <- function(dat, syntax, estimator = "ULSMV", ordered = TRUE) {
  ov <- lavaan::lavNames(lavaan::lavaanify(syntax), "ov")
  missing_vars <- setdiff(ov, names(dat))
  if (length(missing_vars)) {
    stop("The model uses variables that are not selected: ", paste(missing_vars, collapse = ", "))
  }

  warns <- character()
  fit <- withCallingHandlers(
    lavaan::cfa(
      model = syntax,
      data = dat,
      ordered = if (ordered) ov else NULL,
      estimator = estimator
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  m <- lavaan::fitMeasures(fit)
  get_fm <- function(name) {
    sc <- paste0(name, ".scaled")
    if (sc %in% names(m) && !is.na(m[sc])) unname(m[sc]) else if (name %in% names(m)) unname(m[name]) else NA
  }
  fm <- list(
    cfi = get_fm("cfi"), tli = get_fm("tli"), rmsea = get_fm("rmsea"),
    rmsea_lo = get_fm("rmsea.ci.lower"), rmsea_hi = get_fm("rmsea.ci.upper"),
    srmr = get_fm("srmr"), chisq = get_fm("chisq"), df = get_fm("df"), pvalue = get_fm("pvalue")
  )

  fit_table <- data.frame(
    Index = c("CFI", "TLI", "RMSEA", "RMSEA 90% CI", "SRMR", "Chi-square", "df", "p-value"),
    Value = c(sc_fmt(fm$cfi, 3), sc_fmt(fm$tli, 3), sc_fmt(fm$rmsea, 3),
              paste0(sc_fmt(fm$rmsea_lo, 3), " – ", sc_fmt(fm$rmsea_hi, 3)),
              sc_fmt(fm$srmr, 3), sc_fmt(fm$chisq, 2), sc_fmt(fm$df, 0), sc_fmt(fm$pvalue, 3)),
    Guideline = c("≥ .95 good, ≥ .90 acceptable", "≥ .95 good, ≥ .90 acceptable", "≤ .06 good, ≤ .08 acceptable",
                  "upper bound ≤ .10", "≤ .08 good", "sensitive to sample size", "", "> .05 exact fit"),
    Verdict = c(
      if (is.na(fm$cfi)) "" else if (fm$cfi >= .95) "Good" else if (fm$cfi >= .90) "Acceptable" else "Poor",
      if (is.na(fm$tli)) "" else if (fm$tli >= .95) "Good" else if (fm$tli >= .90) "Acceptable" else "Poor",
      if (is.na(fm$rmsea)) "" else if (fm$rmsea <= .06) "Good" else if (fm$rmsea <= .08) "Acceptable" else "Poor",
      if (is.na(fm$rmsea_hi)) "" else if (fm$rmsea_hi <= .10) "Acceptable" else "Poor",
      if (is.na(fm$srmr)) "" else if (fm$srmr <= .08) "Good" else "Poor",
      "", "",
      if (is.na(fm$pvalue)) "" else if (fm$pvalue > .05) "Exact fit not rejected" else "Exact fit rejected"
    ),
    stringsAsFactors = FALSE
  )

  std <- lavaan::standardizedSolution(fit)
  ld <- std[std$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
  names(ld) <- c("Factor", "Item", "Loading", "SE", "p")
  ld$Flag <- ifelse(abs(ld$Loading) >= 1, "Heywood case (|λ| ≥ 1)",
             ifelse(ld$Loading < 0, "Negative loading",
             ifelse(abs(ld$Loading) < .30, "Very weak (< .30)",
             ifelse(abs(ld$Loading) < .40, "Weak (< .40)", ""))))

  latents <- lavaan::lavNames(fit, "lv")
  fcor <- std[std$op == "~~" & std$lhs != std$rhs & std$lhs %in% latents & std$rhs %in% latents,
              c("lhs", "rhs", "est.std")]
  names(fcor) <- c("Factor_1", "Factor_2", "Correlation")

  mi <- tryCatch({
    x <- lavaan::modindices(fit, sort. = TRUE, maximum.number = 10)
    x <- x[, intersect(c("lhs", "op", "rhs", "mi", "epc", "sepc.all"), names(x))]
    x[x$mi > 3.84, , drop = FALSE]
  }, error = function(e) NULL)

  list(
    fit = fit, fm = fm, fit_table = fit_table, loadings = ld, factor_cor = fcor, mi = mi,
    converged = lavaan::lavInspect(fit, "converged"),
    warnings = unique(warns), estimator = estimator, syntax = syntax,
    text = utils::capture.output(lavaan::summary(fit, fit.measures = TRUE, standardized = TRUE))
  )
}

sc_interp_cfa <- function(res) {
  fm <- res$fm
  good <- isTRUE(fm$cfi >= .95 && fm$tli >= .95 && fm$rmsea <= .06 && fm$srmr <= .08)
  ok   <- isTRUE(fm$cfi >= .90 && fm$tli >= .90 && fm$rmsea <= .10)
  verdict <- if (good) "Good overall fit: the hypothesised structure reproduces the observed correlations well."
             else if (ok) "Acceptable fit: the model is reasonable, but some misfit remains."
             else "Poor fit: the hypothesised structure does not reproduce the data well (possible multidimensionality, local dependence or misspecification)."
  weak <- res$loadings$Item[res$loadings$Flag != ""]
  resid <- if (!is.null(res$mi) && nrow(res$mi)) res$mi[res$mi$op == "~~", , drop = FALSE] else NULL
  bullets <- c(
    sprintf("CFI = %s, TLI = %s, RMSEA = %s [%s, %s], SRMR = %s.",
            sc_fmt(fm$cfi, 3), sc_fmt(fm$tli, 3), sc_fmt(fm$rmsea, 3), sc_fmt(fm$rmsea_lo, 3), sc_fmt(fm$rmsea_hi, 3), sc_fmt(fm$srmr, 3)),
    sprintf("Standardized loadings range from <b>%.2f</b> to <b>%.2f</b> (mean %.2f).",
            min(res$loadings$Loading), max(res$loadings$Loading), mean(res$loadings$Loading)),
    if (length(weak)) paste("Items with weak, negative or out-of-range loadings:", sc_item_list(weak)),
    if (!is.null(resid) && nrow(resid)) paste0("Largest suggested residual correlations (modification indices): ",
                                                paste0(resid$lhs, "&ndash;", resid$rhs, " (MI = ", sc_fmt(resid$mi, 1), ")", collapse = ", "),
                                                ". These point to possible local dependence (similar wording or content)."),
    if (nrow(res$factor_cor) && any(abs(res$factor_cor$Correlation) > .85)) "Some factors correlate above .85, which suggests they may not be empirically distinct.",
    if (!isTRUE(res$converged)) "<b>The model did not converge.</b> Results are not trustworthy.",
    if (length(res$warnings)) paste("lavaan warnings:", htmltools::htmlEscape(paste(res$warnings, collapse = " | "))),
    "With ordinal data and ULSMV/WLSMV estimators, CFI/TLI tend to look better and RMSEA worse than with continuous data (Xia &amp; Yang, 2019). Read the indices together."
  )
  sc_callout(if (good) "good" else if (ok) "warn" else "bad", "Confirmatory Factor Analysis",
             paste0("<p><b>", verdict, "</b></p>", sc_bullets(bullets)))
}

# -----------------------------------------------------
# IRT (Rasch family, eRm)
# -----------------------------------------------------

sc_irt_prepare <- function(dat) {
  d <- dat[rowSums(!is.na(dat)) > 1, , drop = FALSE]
  mn <- min(as.matrix(d), na.rm = TRUE)
  if (mn != 0) d <- d - mn
  d
}

sc_irt <- function(dat, model = "auto") {
  d <- sc_irt_prepare(dat)
  type <- sc_data_type(d)
  if (model == "auto") {
    model <- switch(type, binary = "RM", polytomous = "RSM", mixed = "PCM",
                    stop("Rasch models need items with 2 to 7 ordered categories."))
  }
  if (model == "RM" && type != "binary") stop("The dichotomous Rasch Model (RM) requires binary items. Choose RSM or PCM instead.")

  mod <- switch(model, RM = eRm::RM(d), RSM = eRm::RSM(d), PCM = eRm::PCM(d))
  pp  <- tryCatch(eRm::person.parameter(mod), error = function(e) NULL)

  th <- tryCatch({
    if (model == "RM") {
      # dichotomous items have a single location: the item difficulty (eRm stores easiness)
      data.frame(Item = colnames(d), Location = round(-mod$betapar, 3), SE = round(mod$se.beta, 3), row.names = NULL)
    } else {
      t <- eRm::thresholds(mod)$threshtable[[1]]
      data.frame(Item = rownames(t), round(t, 3), check.names = FALSE, row.names = NULL)
    }
  }, error = function(e) NULL)

  disordered <- character()
  if (!is.null(th) && ncol(th) > 3) {
    tcols <- grep("^Threshold", names(th))
    dis <- apply(th[, tcols, drop = FALSE], 1, function(x) any(diff(stats::na.omit(x)) < 0))
    disordered <- th$Item[dis]
  }

  fit <- NULL
  sep <- NA
  targeting <- NA
  if (!is.null(pp)) {
    fit <- tryCatch({
      f <- eRm::itemfit(pp)
      out <- data.frame(
        Item       = names(f$i.infitMSQ),
        Infit_MSQ  = as.numeric(f$i.infitMSQ),
        Outfit_MSQ = as.numeric(f$i.outfitMSQ),
        Infit_Z    = as.numeric(f$i.infitZ),
        Outfit_Z   = as.numeric(f$i.outfitZ),
        stringsAsFactors = FALSE
      )
      out$Flag <- ifelse(out$Infit_MSQ > 1.3 | out$Outfit_MSQ > 1.3, "Underfit (noisy)",
                  ifelse(out$Infit_MSQ < 0.7 | out$Outfit_MSQ < 0.7, "Overfit (redundant)", ""))
      out
    }, error = function(e) NULL)
    sep <- tryCatch(eRm::SepRel(pp)$sep.rel, error = function(e) NA)
    targeting <- tryCatch(
      mean(pp$theta.table[["Person Parameter"]], na.rm = TRUE) - mean(th$Location, na.rm = TRUE),
      error = function(e) NA
    )
  }

  list(
    mod = mod, pp = pp, model = model, type = type,
    n = nrow(d), k = ncol(d), thresholds = th, disordered = disordered,
    fit = fit, sep_rel = sep, targeting = targeting,
    text = utils::capture.output(print(mod))
  )
}

sc_irt_model_label <- function(model) {
  switch(model,
         RM  = "Rasch Model (dichotomous items)",
         RSM = "Rating Scale Model (polytomous items, shared thresholds)",
         PCM = "Partial Credit Model (polytomous items, item-specific thresholds)")
}

sc_interp_irt <- function(res) {
  under <- if (!is.null(res$fit)) res$fit$Item[res$fit$Flag == "Underfit (noisy)"] else character()
  over  <- if (!is.null(res$fit)) res$fit$Item[res$fit$Flag == "Overfit (redundant)"] else character()
  loc <- if (!is.null(res$thresholds)) res$thresholds$Location else NA
  bullets <- c(
    sprintf("Model: <b>%s</b>, estimated on <b>%d respondents</b> and <b>%d items</b> (responses recoded to start at 0).",
            sc_irt_model_label(res$model), res$n, res$k),
    if (!all(is.na(loc))) sprintf("Item locations range from <b>%.2f</b> to <b>%.2f</b> logits (spread %.2f). %s",
                                  min(loc), max(loc), diff(range(loc)),
                                  if (diff(range(loc)) > 2) "The items cover the trait broadly." else "The items cover a fairly narrow range of the trait, so precision is limited outside it."),
    if (!is.na(res$targeting)) sprintf("Targeting: mean person location minus mean item location = <b>%.2f</b> logits. %s",
                                       res$targeting,
                                       if (abs(res$targeting) <= 1) "Items are well targeted to this sample."
                                       else if (res$targeting > 0) "Respondents sit above most items. The scale may be too easy (ceiling effects)."
                                       else "Respondents sit below most items. The scale may be too hard (floor effects)."),
    if (!is.na(res$sep_rel)) sprintf("Person separation reliability = <b>%.2f</b> (%s).", res$sep_rel, reliability_label(res$sep_rel)),
    if (!is.null(res$fit)) sprintf("Mean infit MSQ = %.2f, mean outfit MSQ = %.2f (ideal ≈ 1.0, acceptable 0.7–1.3).",
                                   mean(res$fit$Infit_MSQ), mean(res$fit$Outfit_MSQ)),
    if (length(under)) paste("Underfitting items (MSQ > 1.3: unpredictable responses, possible multidimensionality or ambiguous wording):", sc_item_list(under)),
    if (length(over))  paste("Overfitting items (MSQ < 0.7: overly predictable, possible redundancy or local dependence):", sc_item_list(over)),
    if (length(res$disordered)) paste("Items with <b>disordered thresholds</b> (respondents do not use the categories in the intended order. Consider collapsing categories):", sc_item_list(res$disordered)),
    if (is.null(res$fit)) "Person parameters or item fit could not be estimated (often caused by extreme response patterns or very few items)."
  )
  level <- if (is.null(res$fit)) "warn" else if (length(under) + length(res$disordered) == 0) "good" else if (length(under) <= 2) "warn" else "bad"
  sc_callout(level, "Rasch / IRT model", sc_bullets(bullets))
}

sc_plot_icc <- function(res) {
  mod <- res$mod
  if (res$model == "RM") {
    eRm::plotjointICC(mod, xlab = "Latent trait", main = "Item Characteristic Curves", legpos = "topleft")
  } else {
    n_items <- ncol(mod$X)
    n_col <- min(4, ceiling(sqrt(n_items)))
    n_row <- ceiling(n_items / n_col)
    op <- graphics::par(mfrow = c(n_row, n_col), mar = c(3, 3, 2, 1))
    on.exit(graphics::par(op))
    for (i in seq_len(n_items)) {
      eRm::plotICC(mod, item.subset = i, ask = FALSE, legpos = FALSE)
    }
  }
}

sc_icc_height <- function(k, model) {
  if (identical(model, "RM")) return(500)
  n_col <- min(4, ceiling(sqrt(k)))
  max(400, ceiling(k / n_col) * 230)
}

sc_plot_pi <- function(res) {
  eRm::plotPImap(res$mod, sorted = TRUE)
}

# -----------------------------------------------------
# Mokken monotonicity
# -----------------------------------------------------

sc_mono <- function(dat) {
  X <- stats::na.omit(dat)
  m <- mokken::check.monotonicity(X)
  s <- summary(m)
  tab <- data.frame(Item = rownames(s), apply(s, 2, as.numeric), check.names = FALSE, row.names = NULL)

  tab$Scalability <- ifelse(tab$ItemH >= .50, "Strong", ifelse(tab$ItemH >= .40, "Medium", ifelse(tab$ItemH >= .30, "Weak", "Below .30")))
  tab$Crit_flag <- ifelse(tab$crit > 80, "Serious violation", ifelse(tab$crit > 40, "Possible violation", ""))

  H <- tryCatch({
    h <- mokken::coefH(X, results = FALSE)$H
    as.numeric(gsub("[()]", "", h))
  }, error = function(e) c(NA, NA))

  list(mono = m, table = tab, H = H[1], H_se = H[2], n_used = nrow(X))
}

sc_plot_mono <- function(res) {
  k <- nrow(res$table)
  n_col <- min(4, ceiling(sqrt(k)))
  op <- graphics::par(mfrow = c(ceiling(k / n_col), n_col), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(op))
  for (i in seq_len(k)) {
    try(plot(res$mono, items = i, ask = FALSE), silent = TRUE)
  }
}

sc_interp_mono <- function(res) {
  t <- res$table
  low <- t$Item[t$ItemH < .30]
  serious <- t$Item[t$crit > 80]
  possible <- t$Item[t$crit > 40 & t$crit <= 80]
  H <- res$H
  scale_txt <- if (is.na(H)) "Scale H could not be computed."
    else sprintf("Total scale H = <b>%.2f</b> (SE %.3f) → %s.", H, res$H_se,
                 if (H >= .50) "strong Mokken scale" else if (H >= .40) "medium scale" else if (H >= .30) "weak scale" else "not a Mokken scale (H < .30)")
  bullets <- c(
    scale_txt,
    sprintf("Item scalability (ItemH) ranges from %.2f to %.2f across %d items (%d complete cases).", min(t$ItemH), max(t$ItemH), nrow(t), res$n_used),
    if (length(low)) paste("Items with ItemH below .30 (weakly related to the common trait. Candidates for removal or recoding):", sc_item_list(low)),
    if (length(serious)) paste("Items with <b>serious monotonicity violations</b> (crit > 80):", sc_item_list(serious)),
    if (length(possible)) paste("Items with possible violations (crit 40–80):", sc_item_list(possible)),
    if (!length(serious) && !length(possible)) "No item shows a meaningful monotonicity violation (all crit values ≤ 40).",
    if (any(t$ItemH < 0)) "Negative ItemH values often mean an item is <b>reverse-keyed</b> and should be recoded first."
  )
  level <- if (length(serious) || (!is.na(H) && H < .30)) "bad" else if (length(possible) || length(low)) "warn" else "good"
  sc_callout(level, "Mokken monotonicity", sc_bullets(bullets))
}

# -----------------------------------------------------
# Local independence
# -----------------------------------------------------

sc_np <- function(dat, n = 1000) {
  X <- as.matrix(stats::na.omit(dat))
  if (sc_data_type(as.data.frame(X)) != "binary") {
    stop("T1 and T11 nonparametric tests need dichotomous (0/1) items. For polytomous items, use UVA and Mokken conditional association instead.")
  }
  mn <- min(X)
  if (mn != 0) X <- X - mn
  set.seed(123)
  t1  <- eRm::NPtest(X, n = n, method = "T1")
  t11 <- eRm::NPtest(X, n = n, method = "T11")
  pr <- utils::combn(colnames(X), 2)
  pairs <- data.frame(Item_1 = pr[1, ], Item_2 = pr[2, ], p = as.numeric(t1$prop), stringsAsFactors = FALSE)
  list(
    t1 = t1, t11 = t11, n = n,
    sig_pairs = pairs[pairs$p < .05, , drop = FALSE],
    t11_p = as.numeric(t11$prop),
    text = c(utils::capture.output(print(t1)), "", utils::capture.output(print(t11)))
  )
}

sc_uva <- function(dat, cut_off = 0.25) {
  u <- suppressMessages(EGAnet::UVA(dat, cut.off = cut_off))
  list(uva = u, redundant = u$redundant, cut_off = cut_off, text = utils::capture.output(print(u)))
}

sc_ca <- function(dat) {
  X <- stats::na.omit(dat)
  ca <- mokken::check.ca(X, Windex = TRUE)
  final <- ca$InScale[[length(ca$InScale)]]
  list(ca = ca, flagged = colnames(X)[!final], text = utils::capture.output(print(ca$InScale)))
}

sc_interp_li <- function(np = NULL, uva = NULL, ca = NULL) {
  if (is.null(np) && is.null(uva) && is.null(ca)) {
    return(sc_callout("info", "Local independence summary", "<p>Run at least one of the tests above to get a combined summary.</p>"))
  }
  issues <- 0
  bullets <- character()
  if (!is.null(np)) {
    if (nrow(np$sig_pairs)) {
      issues <- issues + 1
      bullets <- c(bullets, sprintf("<b>T1</b>: %d item pair(s) are more alike than the Rasch model allows (p < .05): %s.",
                                    nrow(np$sig_pairs), paste0(np$sig_pairs$Item_1, "&ndash;", np$sig_pairs$Item_2, collapse = ", ")))
    } else bullets <- c(bullets, "<b>T1</b>: no item pair shows excess similarity.")
    if (isTRUE(np$t11_p < .05)) {
      issues <- issues + 1
      bullets <- c(bullets, sprintf("<b>T11</b>: global test is significant (p = %.3f). Overall, the inter-item correlations differ from Rasch expectations.", np$t11_p))
    } else bullets <- c(bullets, sprintf("<b>T11</b>: global test not significant (p = %.3f).", np$t11_p))
  }
  if (!is.null(uva)) {
    if (length(uva$redundant)) {
      issues <- issues + 1
      bullets <- c(bullets, paste0("<b>UVA</b> (wTO &gt; ", uva$cut_off, ") found redundant items: ",
                                   paste0(vapply(names(uva$redundant), function(i) paste0("<b>", i, "</b> &harr; ", paste(uva$redundant[[i]], collapse = ", ")), character(1)), collapse = "; "),
                                   ". Consider keeping one item per redundant set or combining them."))
    } else bullets <- c(bullets, "<b>UVA</b>: no redundant item pairs.")
  }
  if (!is.null(ca)) {
    if (length(ca$flagged)) {
      issues <- issues + 1
      bullets <- c(bullets, paste("<b>Mokken conditional association</b>: items flagged as locally dependent:", sc_item_list(ca$flagged)))
    } else bullets <- c(bullets, "<b>Mokken conditional association</b>: no locally dependent items.")
  }
  verdict <- if (issues == 0) "No evidence of local dependence in the tests that were run."
             else if (issues == 1) "One test shows local dependence. Review the flagged items' content."
             else "Several tests show local dependence. Expect inflated reliability and possibly distorted dimensionality and IRT estimates."
  sc_callout(if (issues == 0) "good" else if (issues == 1) "warn" else "bad", "Local independence summary",
             paste0("<p><b>", verdict, "</b></p>", sc_bullets(bullets)))
}

# -----------------------------------------------------
# Dimensionality convergence
# -----------------------------------------------------

sc_interp_dims <- function(pa = NULL, ega = NULL, hull = NULL) {
  vals <- c(
    "Parallel Analysis" = if (!is.null(pa)) pa$pa$N_factors_percentiles else NA,
    "EGA"               = if (!is.null(ega)) ega$n_dim else NA,
    "Hull Method"       = if (!is.null(hull)) hull$n_factors else NA
  )
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(NULL)
  rows <- paste0("<li>", names(vals), ": <b>", vals, "</b></li>", collapse = "")
  agree <- length(unique(vals)) == 1
  body <- paste0(
    "<ul>", rows, "</ul>",
    if (length(vals) < 2) "<p>Run more than one method to check whether they agree.</p>"
    else if (agree && vals[1] == 1) "<p>All methods agree on a <b>unidimensional</b> structure. This is strong evidence that a single total score is defensible.</p>"
    else if (agree) sprintf("<p>All methods agree on <b>%d dimensions</b>. Consider scoring sub-scales separately or testing a correlated-factors / bifactor CFA.</p>", vals[1])
    else "<p>The methods <b>disagree</b>. This is common when a strong general factor coexists with minor sub-dimensions. Weigh the evidence together with CFA fit, &omega; hierarchical and item content.</p>"
  )
  sc_callout(if (length(vals) >= 2 && agree) "good" else if (length(vals) >= 2) "warn" else "info",
             "Do the dimensionality methods agree?", body)
}

# =====================================================
# R CODE GENERATORS
# -----------------------------------------------------
# Each returns a character string with runnable code
# that reproduces the analysis with the current settings.
# =====================================================

sc_r_vec <- function(x) {
  inner <- paste0('"', x, '"', collapse = ", ")
  wrapped <- strwrap(inner, width = 66)
  paste0("c(", paste(wrapped, collapse = "\n  "), ")")
}

sc_code_data <- function(source, example = NULL, filename = NULL, vars) {
  load <- if (source == "Example Datasets") {
    if (identical(example, "Simulated Unidimensional (dichotomous data)")) {
      paste(
        "# install.packages(\"birm\") if needed",
        "sim <- birm::simData(n = 1000, v = 10, l = 2, model = \"rasch\", seed = 666)",
        "dat_raw <- as.data.frame(sim$data)",
        sep = "\n"
      )
    } else {
      "dat_raw <- psych::bfi[, c(6:10, 1:5, 11:25)]"
    }
  } else {
    sprintf("dat_raw <- read.csv(\"%s\", header = TRUE)", if (is.null(filename)) "your_data.csv" else filename)
  }
  paste0(
    "# ---- Data ------------------------------------------------\n",
    load, "\n",
    "dat <- dat_raw[, ", sc_r_vec(vars), "]\n"
  )
}

sc_code <- function(key, s) {
  switch(
    key,
    desc = paste(
      "library(psych)",
      "",
      "psych::describe(dat)                       # item descriptives",
      "round(100 * psych::responseFrequency(dat), 1)  # % per category (response.frequencies() in older psych)",
      "round(100 * colMeans(is.na(dat)), 1)       # % missing per item",
      "sum(complete.cases(dat))                   # respondents with complete data",
      sep = "\n"
    ),
    cor = paste(
      "library(psych)",
      "library(corrplot)",
      "",
      sprintf("cors <- psych::corr.test(dat, method = \"%s\", use = \"pairwise\", adjust = \"holm\")", s$cor_method),
      "round(cors$r, 2)",
      "",
      "corrplot::corrplot(",
      "  cors$r, method = \"pie\", type = \"upper\", diag = FALSE,",
      "  p.mat = cors$p, addCoef.col = \"black\", tl.col = \"black\", tl.srt = 45,",
      "  col = colorRampPalette(c(\"#B2182B\", \"white\", \"#2166AC\"))(200)",
      ")",
      sep = "\n"
    ),
    rel = paste(
      "library(psych)",
      "",
      sprintf("alpha_res <- psych::alpha(dat, check.keys = %s)", s$check_keys),
      "alpha_res$total$raw_alpha        # Cronbach's alpha",
      "alpha_res$feldt                  # 95% CI for alpha",
      "alpha_res$item.stats[, \"r.drop\"] # corrected item-rest correlations",
      "alpha_res$alpha.drop$raw_alpha   # alpha if item dropped",
      "",
      "omega_res <- psych::omega(dat, plot = FALSE)",
      "omega_res$omega.tot              # McDonald's omega total",
      "omega_res$omega_h                # omega hierarchical",
      "",
      "psych::glb(dat)$glb.Fa           # greatest lower bound",
      sep = "\n"
    ),
    pa = paste(
      "library(EFA.MRFA)",
      "",
      "pa <- EFA.MRFA::parallelMRFA(",
      "  as.matrix(na.omit(dat)),",
      sprintf("  Ndatsets = %d,", as.integer(s$pa_iter)),
      sprintf("  percent  = %d,", as.integer(s$pa_percent)),
      sprintf("  corr     = \"%s\",", s$pa_corr),
      "  display  = TRUE,",
      "  graph    = TRUE",
      ")",
      "pa$N_factors_percentiles  # factors to retain (percentile criterion)",
      "pa$N_factors_mean         # factors to retain (mean criterion)",
      sep = "\n"
    ),
    ega = paste(
      "library(EGAnet)",
      "",
      sprintf("ega <- EGAnet::EGA(dat, model = \"%s\", algorithm = \"%s\", plot.EGA = TRUE)", s$ega_model, s$ega_algo),
      "summary(ega)",
      "ega$wc                          # item membership per dimension",
      "EGAnet::net.loads(ega)$std      # standardized network loadings",
      sep = "\n"
    ),
    hull = paste(
      "library(EFA.MRFA)",
      "",
      "hull <- EFA.MRFA::hullEFA(",
      "  na.omit(dat),",
      sprintf("  extr       = \"%s\",", s$hull_extr),
      sprintf("  index_hull = \"%s\",", s$hull_index),
      "  display    = TRUE,",
      "  graph      = TRUE",
      ")",
      "hull$n_factors",
      sep = "\n"
    ),
    cfa = paste(
      "library(lavaan)",
      "",
      "model <- '",
      s$cfa_syntax,
      "'",
      "",
      "fit <- lavaan::cfa(",
      "  model,",
      "  data      = dat,",
      if (isTRUE(s$cfa_ordered)) "  ordered   = TRUE,     # treat all items as ordinal" else "  # items treated as continuous",
      sprintf("  estimator = \"%s\"", s$cfa_estimator),
      ")",
      "",
      "summary(fit, fit.measures = TRUE, standardized = TRUE)",
      "lavaan::fitMeasures(fit, c(\"cfi.scaled\", \"tli.scaled\", \"rmsea.scaled\",",
      "                           \"rmsea.ci.lower.scaled\", \"rmsea.ci.upper.scaled\", \"srmr\"))",
      "subset(lavaan::standardizedSolution(fit), op == \"=~\")  # standardized loadings",
      "lavaan::modindices(fit, sort. = TRUE, maximum.number = 10)",
      sep = "\n"
    ),
    irt = paste(
      "library(eRm)",
      "",
      "# keep respondents with at least 2 answers; recode so the lowest category is 0",
      "dat_irt <- dat[rowSums(!is.na(dat)) > 1, ]",
      "dat_irt <- dat_irt - min(dat_irt, na.rm = TRUE)",
      "",
      sprintf("mod <- eRm::%s(dat_irt)   # RM = Rasch, RSM = Rating Scale, PCM = Partial Credit", s$irt_model_resolved),
      "summary(mod)",
      "eRm::thresholds(mod)              # item locations and thresholds",
      "",
      "pp <- eRm::person.parameter(mod)",
      "eRm::itemfit(pp)                  # infit / outfit MSQ",
      "eRm::SepRel(pp)                   # person separation reliability",
      sep = "\n"
    ),
    icc = paste(
      "library(eRm)",
      "",
      "dat_irt <- dat[rowSums(!is.na(dat)) > 1, ]",
      "dat_irt <- dat_irt - min(dat_irt, na.rm = TRUE)",
      sprintf("mod <- eRm::%s(dat_irt)", s$irt_model_resolved),
      "",
      if (identical(s$irt_model_resolved, "RM")) "eRm::plotjointICC(mod, xlab = \"Latent trait\")   # item characteristic curves"
      else "eRm::plotICC(mod, ask = FALSE)        # category characteristic curves, one per item",
      "eRm::plotPImap(mod, sorted = TRUE)    # person-item (Wright) map",
      sep = "\n"
    ),
    mono = paste(
      "library(mokken)",
      "",
      "mono <- mokken::check.monotonicity(na.omit(dat))",
      "summary(mono)",
      "plot(mono, ask = FALSE)               # item rest-score regressions",
      "mokken::coefH(na.omit(dat))           # scale and item H coefficients",
      sep = "\n"
    ),
    np = paste(
      "library(eRm)",
      "",
      "X <- as.matrix(na.omit(dat))   # dichotomous 0/1 items",
      "set.seed(123)",
      sprintf("eRm::NPtest(X, n = %d, method = \"T1\")   # pairwise local dependence", as.integer(s$np_n)),
      sprintf("eRm::NPtest(X, n = %d, method = \"T11\")  # global local dependence", as.integer(s$np_n)),
      sep = "\n"
    ),
    uva = paste(
      "library(EGAnet)",
      "",
      sprintf("uva <- EGAnet::UVA(dat, cut.off = %s)", s$uva_cut),
      "uva$redundant      # redundant item sets",
      "uva$keep_remove    # suggested items to keep / remove",
      sep = "\n"
    ),
    ca = paste(
      "library(mokken)",
      "",
      "ca <- mokken::check.ca(na.omit(dat), Windex = TRUE)",
      "ca$InScale   # items retained at each step (FALSE = locally dependent)",
      "ca$Index     # W indices",
      "ca$Flagged   # flagged item pairs",
      sep = "\n"
    ),
    ""
  )
}

# =====================================================
# METRIC TILES
# =====================================================

sc_metric <- function(label, value, note = "", level = "info", digits = 2, fill = NULL) {
  if (is.null(fill)) fill <- if (is.numeric(value) && !is.na(value)) max(0, min(1, value)) else 0
  sprintf(
    "<div class='sc-metric sc-%s'><div class='sc-metric-label'>%s</div><div class='sc-metric-value'>%s</div><div class='sc-metric-note'>%s</div><div class='sc-metric-bar' style='width:%.0f%%'></div></div>",
    level, label, if (is.numeric(value)) sc_fmt(value, digits) else value, note, 100 * fill
  )
}

sc_metrics <- function(...) {
  paste0("<div class='sc-metrics'>", paste0(..., collapse = ""), "</div>")
}

sc_rel_tiles <- function(res) {
  sc_metrics(
    sc_metric("Cronbach's α", res$alpha, reliability_label(res$alpha), reliability_level(res$alpha)),
    sc_metric("McDonald's ω total", res$omega, reliability_label(res$omega), reliability_level(res$omega)),
    sc_metric("ω hierarchical", res$omega_h,
              if (is.na(res$omega_h)) "Not estimated" else if (res$omega_h >= .70) "Strong general factor" else "Weak general factor",
              if (is.na(res$omega_h)) "info" else if (res$omega_h >= .70) "good" else "warn"),
    sc_metric("Greatest Lower Bound", res$glb, reliability_label(res$glb), reliability_level(res$glb))
  )
}

sc_cfa_tiles <- function(res) {
  fm <- res$fm
  lvl <- function(ok_good, ok_acc) if (isTRUE(ok_good)) "good" else if (isTRUE(ok_acc)) "warn" else "bad"
  sc_metrics(
    sc_metric("CFI", fm$cfi, "≥ .95 good", lvl(fm$cfi >= .95, fm$cfi >= .90), 3),
    sc_metric("TLI", fm$tli, "≥ .95 good", lvl(fm$tli >= .95, fm$tli >= .90), 3),
    sc_metric("RMSEA", fm$rmsea, sprintf("90%% CI %s–%s", sc_fmt(fm$rmsea_lo, 3), sc_fmt(fm$rmsea_hi, 3)),
              lvl(fm$rmsea <= .06, fm$rmsea <= .08), 3, fill = max(0, 1 - fm$rmsea * 5)),
    sc_metric("SRMR", fm$srmr, "≤ .08 good", lvl(fm$srmr <= .08, fm$srmr <= .10), 3, fill = max(0, 1 - fm$srmr * 5))
  )
}
