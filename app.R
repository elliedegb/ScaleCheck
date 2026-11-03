library(shiny)
library(bslib)
library(psych)
library(mokken)
library(EFA.MRFA)
library(EGAnet)
library(lavaan)
library(proxy)
library(corrplot)
library(eRm)
library(rmarkdown)
# Analyses, interpretations and R-code generators live in R/scalecheck_helpers.R
# (Shiny loads the R/ folder automatically).

# =====================================================
# THEME
# =====================================================

sc_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#1f2a37",
  primary = "#2f5d8a",
  secondary = "#6c7a89",
  success = "#2e8b57",
  warning = "#e6a700",
  danger = "#c0392b",
  base_font = font_google("Inter", local = FALSE),
  code_font = font_google("JetBrains Mono", local = FALSE),
  "navbar-bg" = "#1f3b57",
  "card-border-radius" = "14px"
)

# =====================================================
# UI BUILDING BLOCKS
# =====================================================

# Dark code block with a copy button, shown under each analysis
code_block <- function(key) {
  div(
    class = "sc-code",
    div(
      class = "sc-code-header",
      span(icon("code"), "R code to reproduce this analysis"),
      tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-light sc-copy",
        `data-target` = paste0("code_", key),
        icon("copy"), " Copy"
      )
    ),
    verbatimTextOutput(paste0("code_", key))
  )
}

# One card per analysis: header with Run button, collapsible method notes,
# settings, run status, results and the matching R code.
analysis_card <- function(key, title, icon_name, about, ..., refs = NULL, settings = NULL) {
  card(
    class = "sc-card",
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      span(class = "sc-card-title", icon(icon_name), title),
      actionButton(paste0("run_", key), "Run", icon = icon("play"), class = "btn-primary btn-sm sc-run")
    ),
    card_body(
      accordion(
        class = "sc-about",
        open = FALSE,
        accordion_panel(
          "About this method",
          icon = icon("circle-info"),
          about,
          if (!is.null(refs)) div(class = "sc-refs", tags$b("References"), refs)
        )
      ),
      if (!is.null(settings)) div(
        class = "sc-settings",
        div(class = "sc-settings-label", icon("sliders"), " Settings"),
        settings
      ),
      uiOutput(paste0("status_", key)),
      conditionalPanel(
        condition = sprintf("input.run_%s > 0", key),
        div(class = "sc-results", ...)
      ),
      code_block(key)
    )
  )
}

ref <- function(...) p(HTML(paste0(...)))

copy_js <- HTML("
document.addEventListener('click', function (e) {
  const btn = e.target.closest('.sc-copy');
  if (!btn) return;
  const el = document.getElementById(btn.dataset.target);
  if (!el) return;
  navigator.clipboard.writeText(el.innerText).then(function () {
    const old = btn.innerHTML;
    btn.innerHTML = '&#10003; Copied';
    setTimeout(function () { btn.innerHTML = old; }, 1500);
  });
});
")

# =====================================================
# SIDEBAR
# =====================================================

sc_sidebar <- sidebar(
  width = 320,
  open = "desktop",

  div(class = "sc-side-section", icon("database"), " 1 · Data source"),
  radioButtons(
    inputId = "data_source",
    label = NULL,
    choices = c("Example Datasets", "Upload .csv File")
  ),
  conditionalPanel(
    condition = "input.data_source == 'Example Datasets'",
    selectInput(
      inputId = "example_data",
      label = "Example dataset",
      choices = c(
        "Big Five Inventory (polytomous data)",
        "Simulated Unidimensional (dichotomous data)"
      )
    )
  ),
  conditionalPanel(
    condition = "input.data_source == 'Upload .csv File'",
    fileInput(
      inputId = "file",
      label = "Upload your dataset (.csv)",
      accept = ".csv"
    ),
    selectInput(
      inputId = "csv_sep",
      label = "Column separator",
      choices = c("Comma (,)" = ",", "Semicolon (;)" = ";", "Tab" = "\t"),
      selected = ","
    )
  ),

  div(class = "sc-side-section", icon("list-check"), " 2 · Items"),
  selectizeInput(
    inputId = "selected_vars",
    label = NULL,
    choices = NULL,
    multiple = TRUE,
    options = list(plugins = list("remove_button"), placeholder = "Choose items…")
  ),
  div(
    class = "d-flex gap-2",
    actionButton("select_all", "All", icon = icon("check-double"), class = "btn-outline-secondary btn-sm"),
    actionButton("select_none", "Clear", icon = icon("eraser"), class = "btn-outline-secondary btn-sm")
  ),

  div(class = "sc-side-section mt-2", icon("circle-info"), " Current selection"),
  uiOutput("data_summary"),

  div(
    class = "sc-note mt-2",
    icon("play"), " Nothing runs automatically. Open a tab, choose the settings and click ",
    tags$b("Run"), " on each analysis."
  )
)

# =====================================================
# PAGES
# =====================================================

# ---------------- Home ----------------

home_panel <- nav_panel(
  "Home",
  icon = icon("house"),

  div(
    class = "sc-hero",
    h1("ScaleCheck"),
    p(tags$b("Psychometric assumption diagnostics for reflective measurement.")),
    p("Evaluate the reliability, dimensionality and measurement assumptions of a psychological or behavioral instrument in one place,",
      "combining Classical Test Theory, factor analysis, network psychometrics and Rasch / Mokken item response models."),
    div(
      class = "sc-steps",
      span(class = "sc-step", "① Choose or upload data"),
      span(class = "sc-step", "② Select the items"),
      span(class = "sc-step", "③ Run each analysis"),
      span(class = "sc-step", "④ Copy the R code"),
      span(class = "sc-step", "⑤ Download the report")
    )
  ),

  layout_column_wrap(
    width = "260px",
    fill = FALSE,
    card(class = "sc-feature", card_body(
      h5(icon("table"), " Descriptives & correlations"),
      p("Item distributions, missing data, sparse categories, reverse-keyed items and inter-item correlations.")
    )),
    card(class = "sc-feature", card_body(
      h5(icon("shield-halved"), " Reliability"),
      p("Cronbach's α with confidence interval, McDonald's ω total and hierarchical, GLB and item-rest statistics.")
    )),
    card(class = "sc-feature", card_body(
      h5(icon("cubes"), " Dimensionality"),
      p("Parallel Analysis, Exploratory Graph Analysis and the Hull method, with a check of whether they agree.")
    )),
    card(class = "sc-feature", card_body(
      h5(icon("diagram-project"), " Confirmatory Factor Analysis"),
      p("One-factor or custom lavaan models for ordinal data: fit indices, loadings and modification indices.")
    )),
    card(class = "sc-feature", card_body(
      h5(icon("chart-line"), " Rasch / IRT"),
      p("Rasch, Rating Scale and Partial Credit models: item fit, thresholds, targeting and separation reliability.")
    )),
    card(class = "sc-feature", card_body(
      h5(icon("link-slash"), " Model assumptions"),
      p("Monotonicity (Mokken, ICCs) and local independence (T1/T11, UVA, conditional association).")
    ))
  ),

  br(),

  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header(span(class = "sc-card-title", icon("file-csv"), "How your data should look")),
      card_body(
        p("Rows are respondents and columns are items. All response categories must be coded as numbers."),
        tags$ul(
          tags$li("Each row is one participant / respondent."),
          tags$li("Each column is one item, with a clear identifier as its name (e.g. Item1, Q2, Anxiety_3)."),
          tags$li("Ordinal Likert-type items (up to 7 categories) and binary items (0/1) are supported."),
          tags$li("Recode reverse-keyed items before analysis. The Reliability card can flag them for you."),
          tags$li("Do not select participant IDs, demographics or open-ended text as items."),
          tags$li("Inspect missing data and empty or very rare categories before running IRT models.")
        ),
        h6("Example structure"),
        tableOutput("example_table")
      )
    ),
    card(
      card_header(span(class = "sc-card-title", icon("lightbulb"), "Reading the results")),
      card_body(
        p("Each analysis card has the same layout:"),
        tags$ol(
          tags$li(tags$b("About this method:"), " what it does and how to read it (click to expand)."),
          tags$li(tags$b("Settings:"), " the options used for that analysis."),
          tags$li(tags$b("Run:"), " the analysis is only computed when you click it. A badge warns you when the data or settings changed after the last run."),
          tags$li(tags$b("Results:"), " tables and plots with flagged items highlighted, plus an interpretation that adapts to your data."),
          tags$li(tags$b("R code:"), " the exact code to reproduce the output in your own R session.")
        ),
        div(
          class = "sc-callout sc-warn",
          div(class = "sc-callout-title", "⚠ Note"),
          "The thresholds used throughout are practical heuristics. Always evaluate them against the theory, the content of the items and the purpose of the instrument."
        )
      )
    )
  )
)

# ---------------- Descriptives ----------------

descriptives_panel <- nav_panel(
  "Descriptives",
  icon = icon("table"),

  analysis_card(
    "desc", "Item descriptives & data screening", "table",
    about = HTML("
      <p>Summarizes central tendency, variability, distribution shape and missing data for every selected item,
      and the percentage of respondents using each response category.</p>
      <ul>
        <li><b>Skewness / kurtosis</b>: values beyond ±2 / ±7 suggest strongly non-normal items.</li>
        <li><b>Missing %</b>: items with more than 10% missing deserve attention.</li>
        <li><b>Sparse categories</b>: categories used by fewer than 2% of respondents destabilize polychoric correlations, CFA and IRT thresholds.</li>
      </ul>"),
    navset_underline(
      nav_panel("Screening summary", uiOutput("desc_interp")),
      nav_panel("Item statistics", uiOutput("desc_table")),
      nav_panel("Response frequencies", uiOutput("desc_freq"))
    )
  ),

  analysis_card(
    "cor", "Inter-item correlations", "border-all",
    about = HTML("
      <p>Correlation coefficients describe the strength and direction of association between pairs of items.</p>
      <ul>
        <li><b>Spearman</b>: rank-based; suits ordinal (Likert) items and is robust to skewness and outliers.</li>
        <li><b>Kendall</b>: rank-based and more conservative; preferred for small samples or many tied responses.</li>
        <li><b>Pearson</b>: linear association between approximately continuous, normally distributed variables.</li>
      </ul>
      <p>In the plot, <b>blue</b> is positive and <b>red</b> is negative. Fuller pies mean stronger associations,
      and crossed cells are not significant (Holm-adjusted p &ge; .05).
      Negative correlations often point to reverse-keyed items.</p>"),
    settings = selectInput("correlationtype", "Type of correlation", choices = c("spearman", "kendall", "pearson"), selected = "spearman"),
    uiOutput("cor_interp"),
    plotOutput("correlation", height = "560px")
  ),

  analysis_card(
    "rel", "Reliability", "shield-halved",
    about = HTML("
      <p>Reliability is the degree to which the items consistently measure the same construct.</p>
      <ul>
        <li><b>Cronbach's &alpha;</b> assumes all items contribute equally (tau-equivalence).</li>
        <li><b>McDonald's &omega; total</b> lets loadings differ and is usually the preferred coefficient.</li>
        <li><b>&omega; hierarchical</b> is the share of total-score variance due to one general factor. Values &ge; .70 support using a total score.</li>
        <li><b>GLB</b> is the greatest lower bound to reliability. It is useful as a reference but optimistic in small samples.</li>
      </ul>
      <p>Guidelines: &lt; .60 poor · .60–.69 questionable · .70–.79 acceptable · .80–.89 good · &ge; .90 excellent
      (very high values may mean the items are redundant).</p>
      <p>The item table shows the <b>corrected item-rest correlation</b> (values below .30 are weak) and <b>alpha if the item is dropped</b>.</p>"),
    refs = ref("McNeish, D. (2018). Thanks coefficient alpha, we'll take it from here. <i>Psychological Methods, 23</i>(3), 412–433. https://doi.org/10.1037/met0000144"),
    settings = checkboxInput("check_keys", "Automatically reverse negatively keyed items for α (check.keys)", value = FALSE),
    uiOutput("rel_tiles"),
    uiOutput("rel_interp"),
    uiOutput("rel_items")
  )
)

# ---------------- Dimensionality ----------------

dimensionality_panel <- nav_panel(
  "Dimensionality",
  icon = icon("cubes"),

  uiOutput("dims_summary"),

  navset_pill(
    id = "dim_tabs",

    nav_panel(
      "Parallel Analysis",
      br(),
      analysis_card(
        "pa", "Parallel Analysis (MRFA)", "chart-area",
        about = HTML("
          <p>Parallel Analysis compares the variance explained by each factor in the real data with the variance explained in
          random datasets of the same size (<code>EFA.MRFA::parallelMRFA()</code>). A factor is retained when the real data explain more
          common variance than the chosen percentile of the random data.</p>
          <p><b>Polychoric</b> correlations are recommended for ordinal or binary items. More random datasets give more stable
          results but take longer. Rows with missing values are removed (listwise deletion).</p>"),
        refs = ref("Timmerman, M. E., & Lorenzo-Seva, U. (2011). Dimensionality assessment of ordered polytomous items with parallel analysis. <i>Psychological Methods, 16</i>(2), 209–220. https://doi.org/10.1037/a0023353"),
        settings = layout_columns(
          col_widths = c(6, 3, 3),
          sliderInput("pa_iter", "Number of random datasets", value = 50, min = 10, max = 500, step = 10),
          selectInput("pa_percent", "Percentile", choices = c(90, 95, 99), selected = 95),
          selectInput("pa_corr", "Correlation", choices = c("Polychoric", "Pearson"), selected = "Polychoric")
        ),
        uiOutput("pa_interp"),
        plotOutput("pa_plot", height = "420px"),
        accordion(open = FALSE, accordion_panel("Full console output", icon = icon("terminal"), div(class = "sc-console", verbatimTextOutput("parallel_text"))))
      )
    ),

    nav_panel(
      "Exploratory Graph Analysis",
      br(),
      analysis_card(
        "ega", "Exploratory Graph Analysis (EGA)", "circle-nodes",
        about = HTML("
          <p>EGA (<code>EGAnet::EGA()</code>) estimates a regularized partial-correlation network. Items are <b>nodes</b> and conditional
          associations are <b>edges</b>. A community-detection algorithm then finds clusters of densely connected items, and each cluster
          is read as a latent dimension.</p>
          <ul>
            <li>Clearly separated clusters with strong within-cluster edges support a clean structure.</li>
            <li>Items connected to several clusters, or not assigned to any, show ambiguity.</li>
            <li><b>Network loadings</b> are the network analogue of factor loadings.</li>
          </ul>"),
        refs = ref("Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A new approach for estimating the number of dimensions in psychological research. <i>PLoS ONE, 12</i>(6), e0174035. https://doi.org/10.1371/journal.pone.0174035"),
        settings = layout_columns(
          col_widths = c(6, 6),
          selectInput("ega_model", "Network estimation", choices = c("glasso", "TMFG"), selected = "glasso"),
          selectInput("ega_algo", "Community detection", choices = c("walktrap", "louvain", "leiden"), selected = "walktrap")
        ),
        uiOutput("ega_interpretation"),
        layout_columns(
          col_widths = c(7, 5),
          plotOutput("ega_plot", height = "460px"),
          uiOutput("ega_table")
        ),
        accordion(open = FALSE, accordion_panel("Full console output", icon = icon("terminal"), div(class = "sc-console", verbatimTextOutput("ega_text"))))
      )
    ),

    nav_panel(
      "Hull Method",
      br(),
      analysis_card(
        "hull", "Hull Method", "draw-polygon",
        about = HTML("
          <p>The Hull method (<code>EFA.MRFA::hullEFA()</code>) chooses the number of factors with the best balance between
          <b>goodness of fit</b> and <b>model complexity</b> (degrees of freedom). It looks for the elbow on the convex hull of the
          fit-versus-complexity curve, after which extra factors add little fit.</p>
          <p>It complements Parallel Analysis by explicitly rewarding parsimony. Rows with missing values are removed.</p>"),
        refs = ref("Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. L. (2011). The Hull method for selecting the number of common factors. <i>Multivariate Behavioral Research, 46</i>(2), 340–364. https://doi.org/10.1080/00273171.2011.564527"),
        settings = layout_columns(
          col_widths = c(6, 6),
          selectInput("hull_extr", "Extraction method", choices = c("ULS", "ML"), selected = "ULS"),
          selectInput("hull_index", "Fit index", choices = c("CAF", "CFI", "RMSEA"), selected = "CAF")
        ),
        uiOutput("hull_interpretation"),
        plotOutput("hull_plot", height = "420px"),
        accordion(open = FALSE, accordion_panel("Full console output", icon = icon("terminal"), div(class = "sc-console", verbatimTextOutput("hull_text"))))
      )
    )
  )
)

# ---------------- CFA ----------------

cfa_panel <- nav_panel(
  "CFA",
  icon = icon("diagram-project"),

  analysis_card(
    "cfa", "Confirmatory Factor Analysis", "diagram-project",
    about = HTML("
      <p>CFA (<code>lavaan::cfa()</code>) tests whether a theory-driven latent structure reproduces the observed correlations.
      By default a <b>single factor</b> is specified with every selected item loading on it. You can also write your own lavaan model,
      for example two correlated factors.</p>
      <p>For ordinal or binary items, use the robust <b>ULSMV</b> or <b>WLSMV</b> estimators with items declared as ordered. Thresholds
      are then estimated instead of intercepts.</p>
      <ul>
        <li><b>CFI / TLI</b> (comparative fit): &ge; .95 good, &ge; .90 acceptable.</li>
        <li><b>RMSEA</b> (misfit per degree of freedom): &le; .06 good, &le; .08 acceptable.</li>
        <li><b>SRMR</b> (average residual correlation): &le; .08 good.</li>
        <li><b>Chi-square</b>: tests exact fit and is almost always significant in large samples.</li>
        <li><b>Standardized loadings</b>: &ge; .50 acceptable, &ge; .70 strong. Values &lt; .40 are weak indicators.</li>
        <li><b>Modification indices</b> for residual covariances (<code>~~</code>) point to item pairs that share variance beyond the factor (local dependence).</li>
      </ul>"),
    refs = tagList(
      ref("Bollen, K. A. (1989). <i>Structural equations with latent variables</i>. John Wiley & Sons."),
      ref("Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis. <i>Structural Equation Modeling, 6</i>, 1–55. https://doi.org/10.1080/10705519909540118"),
      ref("Xia, Y., & Yang, Y. (2019). RMSEA, CFI, and TLI in structural equation modeling with ordered categorical data. <i>Behavior Research Methods, 51</i>, 409–428. https://doi.org/10.3758/s13428-018-1055-2")
    ),
    settings = tagList(
      layout_columns(
        col_widths = c(4, 4, 4),
        radioButtons("cfa_model_type", "Model", choices = c("One factor (all items)" = "one", "Custom lavaan syntax" = "custom"), selected = "one"),
        selectInput("cfa_estimator", "Estimator", choices = c("ULSMV", "WLSMV", "MLR", "ML"), selected = "ULSMV"),
        checkboxInput("cfa_ordered", "Treat items as ordinal (ordered)", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.cfa_model_type == 'custom'",
        textAreaInput("cfa_syntax", "lavaan model syntax", rows = 4, width = "100%",
                      placeholder = "F1 =~ item1 + item2 + item3\nF2 =~ item4 + item5 + item6"),
        div(class = "sc-note mb-2", "Use only items selected in the sidebar. Factors are correlated by default. MLR / ML need 'Treat items as ordinal' unchecked.")
      )
    ),
    uiOutput("cfa_tiles"),
    uiOutput("cfa_interpretation"),
    navset_underline(
      nav_panel("Standardized loadings", uiOutput("cfa_loadings")),
      nav_panel("Fit indices", uiOutput("cfa_fit_table")),
      nav_panel("Modification indices", uiOutput("cfa_mi")),
      nav_panel("Full lavaan output", div(class = "sc-console", verbatimTextOutput("cfa_output")))
    )
  )
)

# ---------------- IRT ----------------

irt_panel <- nav_panel(
  "Rasch / IRT",
  icon = icon("chart-line"),

  analysis_card(
    "irt", "Rasch-family Item Response Model", "chart-line",
    about = HTML("
      <p>Item Response Theory models the probability of each response as a function of a person's position on the latent trait.
      ScaleCheck fits Rasch-family models with conditional maximum likelihood (<code>eRm</code>):</p>
      <ul>
        <li><b>RM</b>, the Rasch Model, for dichotomous items.</li>
        <li><b>RSM</b>, the Rating Scale Model: polytomous items share the same threshold structure.</li>
        <li><b>PCM</b>, the Partial Credit Model: each polytomous item has its own thresholds.</li>
      </ul>
      <p><b>Preprocessing.</b> Respondents with fewer than two valid answers are removed, and responses are shifted so the lowest category is 0.</p>
      <p><b>Item fit.</b> Infit / outfit mean squares near 1.0 are ideal. Values &gt; 1.3 (underfit) show noisy, unpredictable items, and values &lt; 0.7
      (overfit) show redundant ones. <b>Disordered thresholds</b> mean adjacent categories are not used in the intended order.
      <b>Targeting</b> compares the average person location with the average item location.</p>"),
    refs = ref("Mair, P. (2018). <i>Modern psychometrics with R</i>. Springer International Publishing."),
    settings = selectInput(
      "irt_model", "Model",
      choices = c("Automatic (RM for binary, RSM for polytomous)" = "auto",
                  "Rasch Model (RM)" = "RM", "Rating Scale Model (RSM)" = "RSM", "Partial Credit Model (PCM)" = "PCM"),
      selected = "auto"
    ),
    uiOutput("irt_interpretation"),
    navset_underline(
      nav_panel("Item fit", uiOutput("irt_fit")),
      nav_panel("Item locations & thresholds", uiOutput("irt_thresholds")),
      nav_panel("Model output", div(class = "sc-console", verbatimTextOutput("irt_output")))
    )
  )
)

# ---------------- Monotonicity ----------------

monotonicity_panel <- nav_panel(
  "Monotonicity",
  icon = icon("arrow-trend-up"),

  analysis_card(
    "mono", "Mokken monotonicity test", "arrow-trend-up",
    about = HTML("
      <p>Monotonicity means the probability of endorsing an item (or a higher category) never decreases as the latent trait increases.
      <code>mokken::check.monotonicity()</code> checks it nonparametrically by comparing item scores across rest-score groups.</p>
      <ul>
        <li><b>ItemH</b>: item scalability (&ge; .30 weak, &ge; .40 medium, &ge; .50 strong Mokken scale).</li>
        <li><b>#ac</b>: active pairs, the number of comparisons made. <b>#vi</b>: number of violations. <b>maxvi</b>: largest violation.</li>
        <li><b>zmax</b> / <b>#zsig</b>: z of the largest violation and number of significant violations.</li>
        <li><b>crit</b>: Molenaar's summary criterion. Values &lt; 40 are fine, 40–80 need checking, and &gt; 80 are serious violations.</li>
      </ul>
      <p>The plots show item rest-score regressions, which should rise steadily from left to right.</p>"),
    refs = ref("Franco, V. R., Laros, J. A., & Bastos, R. V. S. (2022). Theoretical and practical foundations of Mokken scale analysis in psychology. <i>Paidéia (Ribeirão Preto), 32</i>, e3223. https://doi.org/10.1590/1982-4327e3223"),
    uiOutput("monotonicity_interpretation"),
    navset_underline(
      nav_panel("Item table", uiOutput("monotonicity_table")),
      nav_panel("Rest-score plots", uiOutput("mono_plot_ui"))
    )
  ),

  analysis_card(
    "icc", "Rasch Item Characteristic Curves & Person–Item map", "wave-square",
    about = HTML("
      <p>A parametric, visual check of monotonicity based on the Rasch-family model chosen in the <b>Rasch / IRT</b> tab.</p>
      <ul>
        <li><b>Binary items</b>: each ICC should be a smooth, increasing S-shaped curve.</li>
        <li><b>Ordinal items</b>: higher categories should become most likely at progressively higher trait levels, with each category
        having its own peak. Hidden or overlapping categories mean the categories are not working as intended.</li>
        <li><b>Person–Item map</b>: persons (top histogram) and item locations / thresholds (bottom) on the same logit scale. Good targeting
        means the items cover the range where the respondents are.</li>
      </ul>"),
    plotOutput("icc", height = "auto"),
    plotOutput("PI", height = "520px")
  )
)

# ---------------- Local independence ----------------

local_panel <- nav_panel(
  "Local Independence",
  icon = icon("link-slash"),

  uiOutput("local_independence_interpretation"),

  analysis_card(
    "np", "Nonparametric Rasch tests (T1 & T11)", "dice",
    about = HTML("
      <p>Quasi-exact tests from <code>eRm::NPtest()</code>. They simulate data that satisfy the Rasch model, then compare them with the observed matrix.
      <b>They only apply to dichotomous items.</b></p>
      <ul>
        <li><b>T1</b> checks every item pair for too many equal responses (increased inter-item correlation).</li>
        <li><b>T11</b> is a global test: it sums the absolute deviations between observed and expected inter-item correlations.</li>
      </ul>
      <p>p &lt; .05 points to local dependence. More samples give more precise p-values but take longer.</p>"),
    refs = ref("Debelak, R., & Koller, I. (2019). Testing the local independence assumption of the Rasch model with Q3-based nonparametric model tests. <i>Applied Psychological Measurement</i>. https://doi.org/10.1177/0146621619835501"),
    settings = sliderInput("np_n", "Number of sampled matrices", min = 100, max = 5000, value = 1000, step = 100),
    uiOutput("np_table"),
    accordion(open = FALSE, accordion_panel("Full console output", icon = icon("terminal"), div(class = "sc-console", verbatimTextOutput("local_independence_np"))))
  ),

  analysis_card(
    "uva", "Unique Variable Analysis (UVA)", "clone",
    about = HTML("
      <p>UVA (<code>EGAnet::UVA()</code>) estimates a network and computes the <b>weighted topological overlap</b> (wTO) between items. Pairs whose
      wTO exceeds the cut-off share so many connections that they are probably redundant (locally dependent), for example
      items with near-identical wording.</p>
      <p>Cut-offs: .20 is liberal, .25 is the default, and .30 flags only large redundancy.</p>"),
    refs = ref("Christensen, A. P., Garrido, L. E., & Golino, H. (2023). Unique variable analysis: A network psychometrics method to detect local dependence. <i>Multivariate Behavioral Research, 58</i>(6), 1165–1182. https://doi.org/10.1080/00273171.2023.2194606"),
    settings = sliderInput("uva_cut", "wTO cut-off", min = 0.15, max = 0.35, value = 0.25, step = 0.05),
    uiOutput("uva_table"),
    accordion(open = FALSE, accordion_panel("Full console output", icon = icon("terminal"), div(class = "sc-console", verbatimTextOutput("local_independence_uva"))))
  ),

  analysis_card(
    "ca", "Mokken conditional association", "people-arrows",
    about = HTML("
      <p><code>mokken::check.ca()</code> uses three special cases of conditional association (W1, W2, W3) to find items that show positive or
      negative local dependence under the monotone homogeneity model. Items are removed one at a time until no flagged item remains,
      and the items dropped along the way are the locally dependent ones.</p>"),
    refs = tagList(
      ref("Straat, J. H., Van der Ark, L. A., & Sijtsma, K. (2016). Using conditional association to identify locally independent item sets. <i>Methodology, 12</i>(4), 117–123. https://doi.org/10.1027/1614-2241/a000115"),
      ref("Sijtsma, K., Van der Ark, L. A., & Straat, J. H. (2015). Goodness-of-fit methods for nonparametric IRT models. In <i>Quantitative psychology research</i> (pp. 109–120). Springer. https://doi.org/10.1007/978-3-319-19977-1_9")
    ),
    uiOutput("ca_result"),
    accordion(open = FALSE, accordion_panel("Full console output", icon = icon("terminal"), div(class = "sc-console", verbatimTextOutput("local_independence_mokken"))))
  )
)

# ---------------- Report ----------------

report_choices <- c(
  "Descriptives & data screening" = "desc",
  "Correlations" = "cor",
  "Reliability" = "rel",
  "Parallel Analysis" = "pa",
  "Exploratory Graph Analysis" = "ega",
  "Hull Method" = "hull",
  "Confirmatory Factor Analysis" = "cfa",
  "Rasch / IRT" = "irt",
  "Monotonicity & ICCs" = "mono",
  "Local independence" = "li"
)

report_panel <- nav_panel(
  "Report",
  icon = icon("file-lines"),

  layout_columns(
    col_widths = c(7, 5),
    card(
      class = "sc-card",
      card_header(span(class = "sc-card-title", icon("file-export"), "Generate an HTML report")),
      card_body(
        p("The report re-runs the selected analyses on the current items, using the ",
          tags$b("settings chosen in each tab"), ". It includes:"),
        tags$ul(
          tags$li("an executive summary with a traffic-light verdict for each assumption"),
          tags$li("a dataset profile, including the settings used"),
          tags$li("tables, plots and adaptive interpretations for every analysis, with flagged items highlighted"),
          tags$li("a list of items worth reviewing, gathered across all methods"),
          tags$li("an appendix with the complete R code and session information, so the results can be reproduced")
        ),
        layout_columns(
          col_widths = c(7, 5),
          textInput("report_title", "Report title", value = "Psychometric Assumption Diagnostics Report"),
          textInput("report_author", "Prepared by", value = "")
        ),
        checkboxGroupInput("report_sections", "Sections to include", choices = report_choices,
                           selected = unname(report_choices), inline = TRUE),
        checkboxInput("report_code", "Include R code appendix", value = TRUE),
        downloadButton("download_report", "Download HTML report", class = "btn-primary btn-lg"),
        div(class = "sc-note mt-2", icon("clock"), " Rendering can take a few minutes for large item sets or many random datasets.")
      )
    ),
    card(
      card_header(span(class = "sc-card-title", icon("code"), "Complete R script")),
      card_body(
        p("All analyses with the current settings in one script. Copy it into R or RStudio."),
        code_block("all")
      )
    )
  )
)

# =====================================================
# UI
# =====================================================

ui <- page_navbar(
  title = span(class = "sc-brand", "ScaleCheck"),
  window_title = "ScaleCheck",
  theme = sc_theme,
  sidebar = sc_sidebar,
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Jaro:opsz@6..72&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/academicons/1.9.4/css/academicons.min.css"),
    tags$link(rel = "stylesheet", href = "scalecheck.css"),
    tags$script(copy_js)
  ),

  home_panel,
  descriptives_panel,
  dimensionality_panel,
  cfa_panel,
  irt_panel,
  monotonicity_panel,
  local_panel,
  report_panel,

  nav_spacer(),
  nav_item(tags$a(icon("globe"), "Website", href = "https://elliedegobi.com/", target = "_blank")),

  footer = div(
    class = "sc-footer",
    tags$b("Developed by Ellie Bastos Degobi"), br(),
    tags$a(href = "https://elliedegobi.com/", target = "_blank", icon("globe"), " elliedegobi.com"),
    tags$a(href = "https://orcid.org/0000-0003-2444-6982", target = "_blank", tags$i(class = "ai ai-orcid"), " ORCID"),
    tags$a(href = "mailto:elliedegb@gmail.com", icon("envelope"), " elliedegb[at]gmail.com"),
    tags$a(href = "https://github.com/elliedegb/ScaleCheck", target = "_blank", icon("github"), " Source code")
  )
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {

  # =================================================
  # DATASET
  # =================================================

  output$example_table <- renderTable({
    data.frame(
      ID = 1:5,
      Item_1 = c(1, 2, 3, 4, 5),
      Item_2 = c(2, 2, 3, 4, 4),
      Item_3 = c(1, 1, 2, 3, 4),
      Item_4 = c(0, 1, 1, 1, 0)
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  dataset <- reactive({
    if (input$data_source == "Example Datasets") {
      sc_example_data(input$example_data)
    } else {
      req(input$file)
      read.csv(input$file$datapath, header = TRUE, sep = input$csv_sep)
    }
  })

  # The selector lives in the UI (not renderUI) so it works even while the sidebar is collapsed
  observeEvent(dataset(), {
    updateSelectizeInput(session, "selected_vars", choices = names(dataset()), selected = head(names(dataset()), 5))
  })

  observeEvent(input$select_all, {
    updateSelectizeInput(session, "selected_vars", selected = names(dataset()))
  })
  observeEvent(input$select_none, {
    updateSelectizeInput(session, "selected_vars", selected = character(0))
  })

  selected_data <- reactive({
    req(input$selected_vars)
    dat <- dataset()[, input$selected_vars, drop = FALSE]
    non_numeric <- names(dat)[!vapply(dat, is.numeric, logical(1))]
    validate(need(!length(non_numeric), paste("Non-numeric columns selected:", paste(non_numeric, collapse = ", "))))
    validate(need(ncol(dat) >= 3, "Select at least 3 items."))
    dat
  })

  output$data_summary <- renderUI({
    dat <- tryCatch(selected_data(), error = function(e) NULL)
    if (is.null(dat)) {
      return(div(class = "sc-datasummary", icon("triangle-exclamation"), " Select at least 3 numeric items."))
    }
    type <- sc_data_type(dat)
    div(
      class = "sc-datasummary",
      div(tags$b(nrow(dat)), " respondents · ", tags$b(ncol(dat)), " items"),
      div(tags$b(sum(complete.cases(dat))), " complete cases"),
      span(class = paste("badge", if (type == "continuous") "bg-warning" else "bg-primary"), sc_type_label(type))
    )
  })

  # =================================================
  # SETTINGS & R CODE
  # =================================================

  data_sig <- reactive(list(input$data_source, input$example_data, input$file$name, input$csv_sep, input$selected_vars))

  cfa_syntax <- reactive({
    if (identical(input$cfa_model_type, "custom") && nzchar(trimws(input$cfa_syntax))) {
      input$cfa_syntax
    } else {
      sc_cfa_syntax(input$selected_vars)
    }
  })

  # Pre-fill the custom CFA box with the one-factor template
  observeEvent(input$cfa_model_type, {
    if (input$cfa_model_type == "custom" && !nzchar(trimws(input$cfa_syntax))) {
      updateTextAreaInput(session, "cfa_syntax", value = sc_cfa_syntax(input$selected_vars))
    }
  })

  irt_model_resolved <- reactive({
    if (input$irt_model != "auto") return(input$irt_model)
    dat <- tryCatch(selected_data(), error = function(e) NULL)
    if (is.null(dat)) return("RSM")
    switch(sc_data_type(dat), binary = "RM", polytomous = "RSM", "PCM")
  })

  settings <- reactive(list(
    cor_method = input$correlationtype,
    check_keys = input$check_keys,
    pa_iter = input$pa_iter,
    pa_percent = as.numeric(input$pa_percent),
    pa_corr = input$pa_corr,
    ega_model = input$ega_model,
    ega_algo = input$ega_algo,
    hull_extr = input$hull_extr,
    hull_index = input$hull_index,
    cfa_syntax = cfa_syntax(),
    cfa_estimator = input$cfa_estimator,
    cfa_ordered = input$cfa_ordered,
    irt_model = input$irt_model,
    irt_model_resolved = irt_model_resolved(),
    np_n = input$np_n,
    uva_cut = input$uva_cut
  ))

  code_preamble <- reactive({
    req(input$selected_vars)
    sc_code_data(input$data_source, input$example_data, input$file$name, input$selected_vars)
  })

  output$code_all <- renderText({
    keys <- c("desc", "cor", "rel", "pa", "ega", "hull", "cfa", "irt", "icc", "mono", "np", "uva", "ca")
    if (irt_model_resolved() != "RM") keys <- setdiff(keys, "np")
    s <- settings()
    paste0(
      "# ScaleCheck — complete analysis script (", Sys.Date(), ")\n",
      "# https://elliedegobi.shinyapps.io/scalecheck/\n\n",
      code_preamble(), "\n",
      paste(vapply(keys, function(k) paste0("# ---- ", k, " ", strrep("-", 50 - nchar(k)), "\n", sc_code(k, s)), character(1)),
            collapse = "\n\n")
    )
  })

  # =================================================
  # RUNNER
  # -------------------------------------------------
  # Each analysis is computed only when its Run button
  # is clicked. The status badge says whether the shown
  # results still match the current data and settings.
  # =================================================

  sc_runner <- function(key, label, compute, settings_sig = function() NULL) {
    run_info <- reactiveVal(NULL)
    result <- reactiveVal(NULL)

    # Computed eagerly on click (not lazily), so results are ready even if the outputs are off screen
    observeEvent(input[[paste0("run_", key)]], {
      sig <- list(data_sig(), settings_sig())
      t0 <- Sys.time()
      out <- withProgress(message = paste0("Running ", label, "…"), value = 0.4, {
        tryCatch(compute(), error = function(e) structure(list(error = e), class = "sc_error"))
      })
      run_info(list(
        sig = sig,
        secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
        at = format(Sys.time(), "%H:%M:%S"),
        error = if (inherits(out, "sc_error")) conditionMessage(out$error) else NULL
      ))
      result(out)
    })

    res <- reactive({
      req(result())
      result()
    })

    output[[paste0("status_", key)]] <- renderUI({
      info <- run_info()
      if (is.null(info)) {
        return(div(class = "sc-status sc-status-idle", icon("circle-play"),
                   HTML("Not run yet. Adjust the settings if needed, then click <b>Run</b>.")))
      }
      if (!is.null(info$error)) {
        return(div(class = "sc-status sc-status-error", icon("circle-xmark"),
                   span(tags$b("The analysis failed: "), info$error)))
      }
      if (!identical(info$sig, list(data_sig(), settings_sig()))) {
        return(div(class = "sc-status sc-status-stale", icon("rotate"),
                   HTML("The items or settings changed since the last run. Click <b>Run</b> to update the results below.")))
      }
      div(class = "sc-status sc-status-ok", icon("circle-check"),
          sprintf("Results up to date · computed at %s in %.1f s", info$at, info$secs))
    })

    output[[paste0("code_", key)]] <- renderText({
      paste0(code_preamble(), "\n", sc_code(key, settings()))
    })

    res
  }

  # Unwrap a result for rendering: blank output until run, nothing on error
  ok <- function(r) {
    x <- r()
    req(!inherits(x, "sc_error"))
    x
  }

  # Read a result without failing when it has not been run
  peek <- function(r) {
    tryCatch({
      x <- r()
      if (inherits(x, "sc_error")) NULL else x
    }, error = function(e) NULL)
  }

  html <- function(x) HTML(x)

  # =================================================
  # DESCRIPTIVES
  # =================================================

  res_desc <- sc_runner("desc", "descriptives", function() sc_descriptives(selected_data()))

  output$desc_interp <- renderUI(html(sc_interp_desc(ok(res_desc))))
  output$desc_table  <- renderUI(html(sc_table_html(ok(res_desc)$table, digits = 2)))
  output$desc_freq   <- renderUI({
    f <- ok(res_desc)$freq
    if (is.null(f)) return(p("Response frequencies are only shown for items with up to 10 categories."))
    html(sc_table_html(f, digits = 1))
  })

  # =================================================
  # CORRELATIONS
  # =================================================

  res_cor <- sc_runner(
    "cor", "correlations",
    function() sc_correlations(selected_data(), input$correlationtype),
    function() input$correlationtype
  )

  output$cor_interp  <- renderUI(html(sc_interp_cor(ok(res_cor))))
  output$correlation <- renderPlot(sc_plot_cor(ok(res_cor)))

  # =================================================
  # RELIABILITY
  # =================================================

  res_rel <- sc_runner(
    "rel", "reliability",
    function() sc_reliability(selected_data(), input$check_keys),
    function() input$check_keys
  )

  output$rel_tiles  <- renderUI(html(sc_rel_tiles(ok(res_rel))))
  output$rel_interp <- renderUI(html(sc_interp_rel(ok(res_rel))))
  output$rel_items  <- renderUI(html(sc_table_html(ok(res_rel)$items, caption = "Item-level reliability statistics")))

  # =================================================
  # PARALLEL ANALYSIS
  # =================================================

  res_pa <- sc_runner(
    "pa", "Parallel Analysis",
    function() sc_parallel(selected_data(), input$pa_iter, as.numeric(input$pa_percent), input$pa_corr),
    function() list(input$pa_iter, input$pa_percent, input$pa_corr)
  )

  output$pa_interp     <- renderUI(html(sc_interp_pa(ok(res_pa))))
  output$pa_plot       <- renderPlot(sc_plot_pa(ok(res_pa)))
  output$parallel_text <- renderText(paste(ok(res_pa)$text, collapse = "\n"))

  # =================================================
  # EGA
  # =================================================

  res_ega <- sc_runner(
    "ega", "Exploratory Graph Analysis",
    function() sc_ega(selected_data(), input$ega_model, input$ega_algo),
    function() list(input$ega_model, input$ega_algo)
  )

  output$ega_interpretation <- renderUI(html(sc_interp_ega(ok(res_ega))))
  output$ega_plot  <- renderPlot(print(plot(ok(res_ega)$ega)))
  output$ega_table <- renderUI(html(sc_table_html(ok(res_ega)$membership, caption = "Item membership & standardized network loadings")))
  output$ega_text  <- renderText(paste(ok(res_ega)$text, collapse = "\n"))

  # =================================================
  # HULL METHOD
  # =================================================

  res_hull <- sc_runner(
    "hull", "the Hull method",
    function() sc_hull(selected_data(), input$hull_extr, input$hull_index),
    function() list(input$hull_extr, input$hull_index)
  )

  output$hull_interpretation <- renderUI(html(sc_interp_hull(ok(res_hull))))
  output$hull_plot <- renderPlot(sc_plot_hull(ok(res_hull)))
  output$hull_text <- renderText(paste(ok(res_hull)$text, collapse = "\n"))

  output$dims_summary <- renderUI({
    x <- sc_interp_dims(peek(res_pa), peek(res_ega), peek(res_hull))
    if (is.null(x)) {
      return(div(class = "sc-callout sc-info",
                 div(class = "sc-callout-title", "ⓘ Dimensionality summary"),
                 "Run Parallel Analysis, EGA and the Hull method. This box then shows whether they agree on the number of dimensions."))
    }
    html(x)
  })

  # =================================================
  # CFA
  # =================================================

  res_cfa <- sc_runner(
    "cfa", "the CFA model",
    function() sc_cfa(selected_data(), cfa_syntax(), input$cfa_estimator, input$cfa_ordered),
    function() list(cfa_syntax(), input$cfa_estimator, input$cfa_ordered)
  )

  output$cfa_tiles          <- renderUI(html(sc_cfa_tiles(ok(res_cfa))))
  output$cfa_interpretation <- renderUI(html(sc_interp_cfa(ok(res_cfa))))
  output$cfa_loadings       <- renderUI({
    r <- ok(res_cfa)
    html(paste0(
      sc_table_html(r$loadings),
      if (nrow(r$factor_cor)) sc_table_html(r$factor_cor, caption = "Factor correlations") else ""
    ))
  })
  output$cfa_fit_table <- renderUI(html(sc_table_html(ok(res_cfa)$fit_table, flag_col = "none")))
  output$cfa_mi        <- renderUI({
    mi <- ok(res_cfa)$mi
    if (is.null(mi) || !nrow(mi)) return(p("No modification index above 3.84 (or they could not be computed for this model)."))
    html(paste0(
      "<p class='sc-note'>The 10 largest modification indices (MI &gt; 3.84). <code>~~</code> rows are residual covariances, which point to local dependence. ",
      "<code>=~</code> rows are cross-loadings. Do not free parameters without a substantive reason.</p>",
      sc_table_html(mi, flag_col = "none")
    ))
  })
  output$cfa_output <- renderText(paste(ok(res_cfa)$text, collapse = "\n"))

  # =================================================
  # IRT
  # =================================================

  res_irt <- sc_runner(
    "irt", "the Rasch model",
    function() sc_irt(selected_data(), input$irt_model),
    function() input$irt_model
  )

  output$irt_interpretation <- renderUI(html(sc_interp_irt(ok(res_irt))))
  output$irt_fit <- renderUI({
    f <- ok(res_irt)$fit
    if (is.null(f)) return(p("Item fit could not be computed."))
    html(sc_table_html(f))
  })
  output$irt_thresholds <- renderUI({
    r <- ok(res_irt)
    th <- r$thresholds
    if (is.null(th)) return(p("Thresholds could not be computed."))
    th$Flag <- ifelse(th$Item %in% r$disordered, "Disordered thresholds", "")
    html(sc_table_html(th, caption = "Item locations (mean of thresholds) and category thresholds, in logits"))
  })
  output$irt_output <- renderText(paste(ok(res_irt)$text, collapse = "\n"))

  # =================================================
  # MONOTONICITY
  # =================================================

  res_mono <- sc_runner("mono", "the monotonicity check", function() sc_mono(selected_data()))

  output$monotonicity_interpretation <- renderUI(html(sc_interp_mono(ok(res_mono))))
  output$monotonicity_table <- renderUI({
    t <- ok(res_mono)$table
    t$Flag <- ifelse(t$crit > 40 | t$ItemH < .30, "Check", "")
    html(sc_table_html(t))
  })
  output$mono_plot_ui <- renderUI({
    k <- nrow(ok(res_mono)$table)
    plotOutput("mono_plot", height = paste0(max(380, ceiling(k / min(4, ceiling(sqrt(k)))) * 240), "px"))
  })
  output$mono_plot <- renderPlot(sc_plot_mono(ok(res_mono)))

  res_icc <- sc_runner(
    "icc", "the Rasch model for ICCs",
    function() sc_irt(selected_data(), input$irt_model),
    function() input$irt_model
  )

  output$icc <- renderPlot(
    sc_plot_icc(ok(res_icc)),
    height = function() {
      r <- peek(res_icc)
      if (is.null(r)) 1 else sc_icc_height(r$k, r$model)
    }
  )
  output$PI <- renderPlot(sc_plot_pi(ok(res_icc)))

  # =================================================
  # LOCAL INDEPENDENCE
  # =================================================

  res_np <- sc_runner(
    "np", "T1 / T11 tests",
    function() sc_np(selected_data(), input$np_n),
    function() input$np_n
  )
  output$np_table <- renderUI({
    r <- ok(res_np)
    html(paste0(
      sprintf("<p><b>T11 global test:</b> p = %.3f %s</p>", r$t11_p, if (r$t11_p < .05) "(significant: local dependence)" else "(not significant)"),
      if (nrow(r$sig_pairs)) sc_table_html(transform(r$sig_pairs, Flag = "p < .05"), caption = "T1: item pairs with significant local dependence")
      else "<p><b>T1:</b> no item pair shows significant local dependence.</p>"
    ))
  })
  output$local_independence_np <- renderText(paste(ok(res_np)$text, collapse = "\n"))

  res_uva <- sc_runner(
    "uva", "Unique Variable Analysis",
    function() sc_uva(selected_data(), input$uva_cut),
    function() input$uva_cut
  )
  output$uva_table <- renderUI({
    r <- ok(res_uva)
    if (!length(r$redundant)) return(html(sc_callout("good", "No redundancy", "<p>No item pair exceeds the wTO cut-off.</p>")))
    tab <- data.frame(
      Target_item = names(r$redundant),
      Redundant_with = vapply(r$redundant, paste, character(1), collapse = ", "),
      Flag = "Redundant"
    )
    html(sc_table_html(tab, caption = "Redundant item sets"))
  })
  output$local_independence_uva <- renderText(paste(ok(res_uva)$text, collapse = "\n"))

  res_ca <- sc_runner("ca", "conditional association", function() sc_ca(selected_data()))
  output$ca_result <- renderUI({
    r <- ok(res_ca)
    if (!length(r$flagged)) return(html(sc_callout("good", "No locally dependent items", "<p>All items stay in the scale after the W1–W3 checks.</p>")))
    html(sc_callout("warn", "Locally dependent items", paste("<p>Items removed during the procedure:", sc_item_list(r$flagged), "</p>")))
  })
  output$local_independence_mokken <- renderText(paste(ok(res_ca)$text, collapse = "\n"))

  output$local_independence_interpretation <- renderUI({
    html(sc_interp_li(peek(res_np), peek(res_uva), peek(res_ca)))
  })

  # =================================================
  # REPORT
  # =================================================

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("ScaleCheck_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      withProgress(message = "Generating psychometric report…", value = 0, {

        incProgress(0.1, detail = "Preparing report template…")
        temp_dir <- file.path(tempdir(), paste0("sc_report_", as.integer(Sys.time())))
        dir.create(temp_dir, showWarnings = FALSE)
        temp_report <- file.path(temp_dir, "Report.Rmd")
        file.copy("Report.Rmd", temp_report, overwrite = TRUE)

        incProgress(0.2, detail = "Collecting data and settings…")
        s <- settings()
        sections <- input$report_sections
        code_keys <- c(
          intersect(c("desc", "cor", "rel", "pa", "ega", "hull", "cfa", "irt"), sections),
          if ("mono" %in% sections) c("mono", "icc"),
          if ("li" %in% sections) c(if (irt_model_resolved() == "RM") "np", "uva", "ca")
        )
        code <- paste0(
          code_preamble(), "\n",
          paste(vapply(code_keys, function(k) sc_code(k, s), character(1)), collapse = "\n\n")
        )

        params <- list(
          data = selected_data(),
          data_label = if (input$data_source == "Example Datasets") input$example_data else input$file$name,
          settings = s,
          sections = sections,
          code = if (isTRUE(input$report_code)) code else NULL,
          title = input$report_title,
          author = input$report_author,
          helpers = normalizePath("R/scalecheck_helpers.R", winslash = "/"),
          css = normalizePath("www/scalecheck.css", winslash = "/")
        )

        incProgress(0.3, detail = "Running analyses and rendering HTML…")
        rmarkdown::render(
          input = temp_report,
          output_format = "html_document",
          output_file = basename(file),
          output_dir = dirname(file),
          params = params,
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        incProgress(0.4, detail = "Done!")
      })
    }
  )
}

# =====================================================
# RUN APP
# =====================================================

shinyApp(ui = ui, server = server)
