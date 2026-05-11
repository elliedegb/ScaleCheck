library(shiny)
library(psych)
library(mokken)
library(EFA.MRFA)
library(EGAnet)
library(lavaan)
library(proxy)
library(birm)
library(corrplot)
library(eRm)
library(rmarkdown)
# =====================================================
# UI
# =====================================================

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Jaro:opsz@6..72&display=swap"
    )
  ),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    )
  ),
  
  tags$head(
    
    # Font Awesome
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    ),
    
    # Academicons (ORCID, Google Scholar, etc.)
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/academicons/1.9.4/css/academicons.min.css"
    )
  ),
  tags$head(
    tags$title("ScaleCheck")
  ),
  titlePanel(
    div(
      style = "
      text-align:center;
      font-family:'Jaro', serif;
      font-size:40px;
      font-weight:bold;
      color:#2C3E50;
      margin-bottom:10px;
    ",
      
      "ScaleCheck: Psychometric Assumption Diagnostics for Reflective Measurement"
    )
  ),
  tags$head(
    tags$style(HTML("
  
  /* =========================
     MAIN NAVBAR (TOP LEVEL)
     ========================= */
     
  .nav-tabs {
    background-color: #e6e6e6 !important;
    border-radius: 8px;
    padding: 5px;
  }

  .nav-tabs > li > a {
    color: #333333 !important;
    font-weight: 600;
    border-radius: 6px;
  }

  .nav-tabs > li > a:hover {
    background-color: #d0d0d0 !important;
  }

  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:focus,
  .nav-tabs > li.active > a:hover {
    background-color: #bdbdbd !important;
    color: black !important;
    border: 1px solid #bdbdbd !important;
  }

  "))
  ),

 
  
  tags$hr(),
  
  div(
    style = "
    text-align:center;
    padding:18px;
    margin-top:18px;
    background-color:#F8F9FA;
    border-top:1px solid #DADCE0;
    border-radius:12px;
    font-size:15px;
    line-height:1.8;
  ",
    
    HTML("
    <b>Developed by Ellie Bastos Degobi</b><br>
    
    <a href='https://orcid.org/0000-0003-2444-6982'
       target='_blank'
       style='text-decoration:none; color:#2C3E50;'>
       <i class='ai ai-orcid ai-1x'></i>
       ORCID Profile 
    </a>
    
    <a href='mailto:elliedegb[at]gmail.com' target='_blank'
       style='text-decoration:none; color:#2C3E50;'>
       <i class='fa fa-envelope'></i>
       elliedegb[at]gmail.com
    </a>
    
    <br>
    <a href='https://elliedegb.github.io/website/'
       target='_blank'
       style='text-decoration:none; color:#2C3E50;'>
       <i class='fa fa-globe'></i>
       Personal Website
    </a>
    <a href='https://github.com/elliedegb/ScaleCheck'
       target='_blank'
       style='text-decoration:none; color:#2C3E50;'>
       <i class='fa fa-github'></i>
       Source Code  
    </a>
  ")
  ),
  
  div(
    style = "
    background-color:#EAF4FF;
    border:1px solid #B3D7FF;
    border-radius:10px;
    padding:20px;
    margin-bottom:18px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.08);
    font-size:13px;
    line-height:1.6;
  ",
    
    HTML("
<h4>About this application</h4>

<p>
This application provides an integrated psychometric diagnostics environment for evaluating the quality, structure, and measurement assumptions of psychological and behavioral instruments.
</p>

<p>
The workflow combines both <b>Classical Test Theory (CTT)</b> and <b>modern latent variable approaches</b> to provide a comprehensive overview of scale performance.
</p>

<ul>

<li>
<b>Descriptive statistics and correlation analysis</b> are used to examine item distributions, variability, missing data patterns, and inter-item relationships.
</li><br>

<li>
<b>Reliability analyses</b> include Cronbach’s alpha, McDonald’s omega, and the Greatest Lower Bound (GLB), allowing evaluation of internal consistency under different psychometric assumptions.
</li><br>

<li>
<b>Dimensionality assessment</b> is conducted using Parallel Analysis, Exploratory Graph Analysis (EGA), and the Hull Method, providing complementary evidence regarding the latent structure of the instrument.
</li><br>

<li>
<b>Confirmatory Factor Analysis (CFA)</b> enables the evaluation of theoretically specified latent models and global model fit indices.
</li><br>

<li>
<b>Item Response Theory (IRT)</b> analyses based on Rasch-family models evaluate item functioning, category structure, item fit, and respondent positioning along the latent trait continuum.
</li><br>

<li>
<b>Measurement assumptions</b> such as monotonicity and local independence are assessed using Mokken scaling procedures, residual dependence diagnostics, and network-based redundancy analyses.
</li>

</ul>

<p>
Together, these methods provide a unified framework for determining whether a set of items forms a reliable, interpretable, and structurally coherent measurement instrument.
</p>
")
  ),
  sidebarLayout(
    
    # =================================================
    # SIDEBAR
    # =================================================
    
    sidebarPanel(
      
      div(
        style = "
    background-color:#f8f9fa;
    border:1px solid #d6dce5;
    border-radius:12px;
    padding:18px;
    box-shadow:0px 2px 6px rgba(0,0,0,0.08);
    ",
        
        tags$h3(
          "Data Source",
          style = "
      text-align:center;
      font-weight:700;
      color:#2C3E50;
      margin-top:0px;
      margin-bottom:18px;
      font-family:'Segoe UI', sans-serif;
      "
        ),
        
        tags$p(
          "Select an example dataset or upload your own .csv file for psychometric analysis.",
          style = "
      font-size:13px;
      color:#5c6773;
      text-align:center;
      margin-bottom:18px;
      "
        ),
        
        radioButtons(
          inputId = "data_source",
          label = tags$span(
            "Choose data source:",
            style = "
        font-weight:600;
        color:#34495E;
        "
          ),
          choices = c(
            "Example Datasets",
            "Upload .csv File"
          )
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'Example Datasets'",
          
          div(
            style = "
        background-color:white;
        padding:12px;
        border-radius:10px;
        border:1px solid #dee2e6;
        margin-top:10px;
        ",
            
            selectInput(
              inputId = "example_data",
              label = tags$span(
                "Select example dataset:",
                style = "font-weight:600;"
              ),
              choices = c(
                "Big Five Inventory (polytomous data)",
                "Simulated Unidimensional (dichotomous data)"
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'Upload .csv File'",
          
          div(
            style = "
        background-color:white;
        padding:12px;
        border-radius:10px;
        border:1px solid #dee2e6;
        margin-top:10px;
        ",
            
            fileInput(
              inputId = "file",
              label = tags$span(
                "Upload your dataset (.csv)",
                style = "font-weight:600;"
              ),
              accept = ".csv"
            )
          )
        )
      ),
      
      br(),
      
      div(
        style = "
    background-color:#f8f9fa;
    border:1px solid #d6dce5;
    border-radius:12px;
    padding:18px;
    box-shadow:0px 2px 6px rgba(0,0,0,0.08);
    ",
        
        h4(
          "Variables",
          style = "
      text-align:center;
      font-weight:700;
      color:#2C3E50;
      margin-bottom:15px;
      "
        ),
        
        uiOutput("variable_selector")
      ),
    
      hr(),
      
      div(
        style = "
    background-color:#f8f9fa;
    padding:15px;
    border-radius:10px;
    border:1px solid #d6d6d6;
  ",
        
        h4("Generate Report"),
        
        p("Download a HTML report containing all psychometric analyses and interpretations."),
        
        downloadButton(
          outputId = "download_report",
          label = "Download HTML Report",
          class = "btn-primary"
        ),
        p("This might take a few minutes.")
        )
      ),
    
    # =================================================
    # MAIN PANEL
    # =================================================
    
    mainPanel(
      
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          
          "How Your Data Should Look",
          
          fluidPage(
            
            br(),
            
            h3("Dataset Structure Requirements"),
            
            p("
      This application was designed for psychometric datasets in which
      rows represent respondents and columns represent items.
    "),
            
            br(),
            
            h4("Recommended Data Format"),
            
            tags$ul(
              tags$li("Inspect missing data patterns before analysis."),
              
              tags$li("Remove duplicated respondents if necessary."),
              
              tags$li("Inspect response distributions for empty or extremely rare categories."),
              
              tags$li("Each row should correspond to one participant/respondent."),
              
              tags$li("Each column should correspond to one item or variable."),
              
              tags$li("Ensure response categories are coded numerically."),
              
              tags$li("Ordinal Likert-type items are fully supported."),
              
              tags$li("Binary items (0/1, Yes/No recoded numerically) are also supported."),
              
              tags$li("Column names should contain item identifiers (e.g., Item1, Q2, Anxiety_3).")
            ),
            
            br(),
            
            h4("Example Structure"),
            
            tableOutput("example_table"),
            
            br(),
            
            h4("Important Notes"),
            
            tags$ul(
              
              tags$li("Do not include participant IDs as analysis variables."),
              
              tags$li("Avoid text responses or open-ended questions."),
              
              tags$li("For Rasch binary models, items should contain only two response categories."),
              
              tags$li("For polytomous IRT models, ordered response categories are recommended.")
            )
  
          )
        ),
        
        # =================================================
        # DESCRIPTIVES
        # =================================================
        
        tabPanel(
          "Descriptive Statistics",
          
          br(),
          
          h2("Item Descriptives"),
          
          tableOutput("selected_table"),
          
          h2("Item Correlations"),
          div(
            style = "
    background-color:#F8F9FA;
    border:1px solid #DADCE0;
    border-radius:10px;
    padding:15px;
    margin-top:10px;
    margin-bottom:20px;
    font-size:14px;
    line-height:1.6;
  ",
            
            HTML("
  <b>Correlation Analysis</b><br><br>

  Correlation coefficients evaluate the strength and direction of association between pairs of variables. Different correlation methods are appropriate for different types of data and assumptions.<br><br>

  <b>1. Pearson Correlation</b><br>
  Pearson’s correlation measures the linear association between continuous variables. It assumes approximately interval-scale data and is sensitive to outliers and non-normal distributions.<br><br>

  Recommended when:
  <ul>
    <li>Variables are continuous</li>
    <li>Relationships are approximately linear</li>
    <li>Data are reasonably normally distributed</li>
  </ul>

  <b>2. Spearman Correlation</b><br>
  Spearman’s correlation is a rank-based, nonparametric coefficient that evaluates monotonic relationships between variables. It is more robust to outliers and appropriate for ordinal data.<br><br>

  Recommended when:
  <ul>
    <li>Items are ordinal (e.g., Likert scales)</li>
    <li>Relationships may not be strictly linear</li>
    <li>Data contain outliers or skewness</li>
  </ul>

  <b>3. Kendall Correlation</b><br>
  Kendall’s tau is another rank-based correlation coefficient that estimates association through concordant and discordant item pairs. It is generally more conservative and stable in small samples or datasets with many tied ranks.<br><br>

  Recommended when:
  <ul>
    <li>Sample sizes are smaller</li>
    <li>Data contain many tied responses</li>
    <li>A robust ordinal association estimate is desired</li>
  </ul>

  <b>Correlation Plot (Corrplot)</b><br>
  The correlation matrix is visualized using the <code>corrplot</code> package.<br><br>

  In the plot:
  <ul>
    <li><b>Blue colors</b> indicate positive correlations</li>
    <li><b>Red colors</b> indicate negative correlations</li>
    <li><b>Larger and more filled symbols</b> indicate stronger associations</li>
    <li><b>Smaller or pale symbols</b> indicate weaker associations</li>
    <li><b>Crossed numbers</b> indicate non-significant associations</li>

  </ul>
  ")
          ),
          selectInput(
            inputId = "correlationtype",
            label = "Type of Correlation:",
            choices = c("spearman", "kendall", "pearson"),
            selected = "spearman"
          ),
          
          br(),
          plotOutput("correlation"),
          br(),
          
          h2("Instrument Reliability"),
          div(
            style = "
    background-color:#F4F8FB;
    border:1px solid #D6E4F0;
    border-radius:10px;
    padding:18px;
    margin-top:15px;
    margin-bottom:20px;
    line-height:1.7;
    font-size:14px;
  ",
            
            HTML("
  <b>Reliability Indices</b><br><br>

  Reliability refers to the degree to which a set of items consistently measures the same latent construct. Higher reliability indicates that item responses share common variance and produce stable measurement.<br><br>

  This application reports three complementary reliability estimates:<br><br>

  <b>1. Cronbach’s Alpha (α)</b><br>
  Cronbach’s alpha is the most widely used estimate of internal consistency reliability. It evaluates how strongly items correlate with one another under the assumption that all items contribute equally to the construct (tau-equivalence).<br><br>

  Interpretation guidelines:
  <ul>
    <li><b>&lt; .60</b> → poor internal consistency</li>
    <li><b>.60 – .69</b> → questionable reliability</li>
    <li><b>.70 – .79</b> → acceptable reliability</li>
    <li><b>.80 – .89</b> → good reliability</li>
    <li><b>≥ .90</b> → excellent reliability (although extremely high values may suggest item redundancy)</li>
  </ul>

  <b>2. McDonald’s Omega (ω)</b><br>
  McDonald’s omega is a model-based reliability estimate that accounts for differences in item loadings. Unlike alpha, omega does not assume equal item contributions and is generally considered a more accurate estimate of reliability when items vary in strength.<br><br>

  Omega is particularly useful when:
  <ul>
    <li>Factor loadings differ substantially across items</li>
    <li>The scale may not satisfy tau-equivalence assumptions</li>
    <li>A latent variable framework is preferred</li>
  </ul>

  In many psychometric applications, omega is recommended as the preferred reliability coefficient.<br><br>

  <b>3. Greatest Lower Bound (GLB)</b><br>
  The Greatest Lower Bound estimates the theoretical lower bound of reliability using the covariance structure of the data.<br><br>

  Although computationally more complex, GLB can provide useful information about the maximum plausible lower-bound reliability of a scale.<br><br>

  <b>Interpreting the visual indicators</b><br>
  The reliability boxes fill from red to blue according to the magnitude of the coefficient:
  <ul>
    <li><b>Lower values</b> → weaker internal consistency</li>
    <li><b>Higher values</b> → stronger consistency among items</li>
  </ul>
  ")
),
          uiOutput("reliability"),
          
p(HTML("<b>For more information, see:</b><br>
McNeish, D. (2018). Thanks coefficient alpha, we’ll take it from here. <i>Psychological methods, 23</i>(3), 412-433."))
          ),
        
        # =================================================
        # DIMENSIONALITY
        # =================================================
        
        tabPanel(
          "Dimensionality",
          
          tabsetPanel(
            
            # -------------------------------------------
            # Parallel Analysis
            # -------------------------------------------
            
            tabPanel(
              "Parallel Analysis",
            
              
              h2("Parallel Analysis"),
              br(),
              
              p(
                HTML("
                  <b>Parallel Analysis (EFA.MRFA)</b> is a simulation-based method used to determine the number of latent dimensions to retain in a dataset by comparing observed eigenvalues to those obtained from random data.<br><br>
                  
                  In this application, Parallel Analysis is implemented using <code>EFA.MRFA::parallelMRFA()</code>. The procedure generates multiple random datasets (<code>Ndatsets</code>) with the same structure as the observed data and computes eigenvalues for each simulated dataset.<br><br>
                  
                  The observed eigenvalues are then compared to the distribution of eigenvalues from the random data. Only factors whose eigenvalues exceed the chosen percentile threshold (here <b>95%</b>) are retained, providing an empirical rule for dimensionality selection.<br><br>
                  
                  The correlation matrix used in this analysis is based on <b>polychoric correlations</b>, which are appropriate for ordinal or categorical items, as they better approximate the relationships between underlying continuous latent variables.<br><br>
                  
                  The printed output displays:
                  <ul>
                  <li>The observed eigenvalues</li>
                  <li>The simulated eigenvalue distribution</li>
                  <li>The recommended number of factors to retain</li>
                  </ul>
                  
                  Parallel Analysis provides a robust empirical criterion for determining dimensionality by directly benchmarking observed structure against chance-level data.
                  ")
              ),
              
              br(),
              
              sliderInput(
                inputId = "pa_iter",
                label = "Number of iterations:",
                value = 20,
                min = 10,
                max = 500,
                step = 50
              ),
              
                
              br(),
              
              verbatimTextOutput("parallel_text"),
              
              p(HTML("<b>For more information, see:</b><br>
                     Timmerman, M. E., & Lorenzo-Seva, U. (2011). Dimensionality assessment of ordered polytomous items with parallel analysis. <i>Psychological Methods, 16</i>(2), 209-220. https://doi.org/10.1037/a0023353"))
            ),
            
            # -------------------------------------------
            # EGA
            # -------------------------------------------
            
            tabPanel(
              "Exploratory Graph Analysis",
              
              br(),
              h2("Exploratory Graph Analysis (EGA)"),
              p(
          HTML("
                <b>Exploratory Graph Analysis (EGA)</b> is a network-based approach to dimensionality assessment that estimates the number of latent variables underlying a set of items by modeling their conditional dependence structure as a network.<br><br>
                
                In this application, EGA is implemented using <code>EGAnet::EGA()</code>, which constructs a regularized partial correlation network from the observed data. Items are represented as nodes, and edges reflect conditional associations between items after controlling for all other variables in the set.<br><br>
                
                Community detection algorithms are then applied to identify clusters of strongly connected items, which are interpreted as latent dimensions.<br><br>
                
                The printed output shows the estimated dimensional structure, including the number of detected dimensions and item membership within each dimension.<br><br>
                
                The corresponding plot provides a visual representation of the network structure:
                <ul>
                <li><b>Nodes</b> represent items</li>
                <li><b>Edges</b> represent partial correlations</li>
                <li><b>Clusters (communities)</b> represent latent dimensions</li>
                </ul>
                
                Visual inspection of the plot helps evaluate:
                <ul>
                <li>Whether item clusters are clearly separated (supporting multidimensional structure)</li>
                <li>Whether items show strong within-cluster connectivity (supporting coherence of factors)</li>
                <li>Whether cross-loadings or weak connections suggest model ambiguity</li>
                </ul>
                
                Together, the numerical output and the network visualization provide complementary information about the latent structure of the dataset.
                "))
          ,
              verbatimTextOutput("ega_text"),

              plotOutput("ega_plot"),
          
         
          div(
            style = "
    background-color: #FFF8DC;
    border-left: 6px solid #E6B800;
    padding: 15px;
    border-radius: 10px;
    margin-top: 15px;
    margin-bottom: 15px;
    color: #333333;
    font-size: 15px;
    line-height: 1.6;
    box-shadow: 0 2px 5px rgba(0,0,0,0.08);
  ",
            uiOutput("ega_interpretation")),
          
          p(HTML("<b>For more information, see:</b><br>
                 Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A new approach for estimating the number of dimensions in psychological research. <i>PloS one, 12</i>(6), e0174035. https://doi.org/10.1371/journal.pone.0174035
          "))
            ),
            
            # -------------------------------------------
            # Hull Method
            # -------------------------------------------
            
            tabPanel(
              "Hull Method",
              
              br(),
              h2("Hull Method"),
              p(
                HTML("
                <b>Hull Method (EFA.MRFA)</b> is a model selection procedure used to determine the optimal number of factors by balancing model fit and parsimony.<br><br>
                
                In this application, the Hull Method is implemented using <code>EFA.MRFA::hullEFA()</code>. The method evaluates a range of factor solutions and identifies the point at which adding additional factors produces diminishing improvements in model fit relative to model complexity.<br><br>
                
                The procedure explicitly searches for the “hull” in the fit-versus-complexity curve — the region where the trade-off between goodness-of-fit and parsimony is optimal.<br><br>
                
                The output provides:
                <ul>
                <li>Candidate factor solutions evaluated across different model sizes</li>
                <li>Fit indices used to compare solutions</li>
                <li>The recommended number of factors based on the optimal trade-off</li>
                </ul>
                
                In the graphical output, the method displays the fit-function curve, allowing visual identification of the optimal point where improvements in fit begin to level off.<br><br>
                
                Overall, the Hull Method complements Parallel Analysis by focusing not only on statistical adequacy but also on model simplicity, prioritizing solutions that achieve strong fit with the fewest possible factors.
                ")
              ),
              
              verbatimTextOutput("hull_text"),
              
              plotOutput("hull_plot"),
              div(
                style = "
    background-color: #FFF8DC;
    border-left: 6px solid #E6B800;
    padding: 15px;
    border-radius: 10px;
    margin-top: 15px;
    margin-bottom: 15px;
    color: #333333;
    font-size: 15px;
    line-height: 1.6;
    box-shadow: 0 2px 5px rgba(0,0,0,0.08);
  ",uiOutput("hull_interpretation")),
              p(HTML("<b>For more information sees:</b><br>
                     Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. (2011). The Hull Method for Selecting the Number of Common Factors. <i>Multivariate Behavioral Research, 46</i>(2), 340-364. https://doi.org/10.1080/00273171.2011.564527")),
              
              ),
            
            # -------------------------------------------
            # CFA
            # -------------------------------------------
            
            tabPanel(
              "Confirmatory Factor Analysis",
              
              h2("Confirmatory Factor Analysis"),
              
              br(),
              HTML(
                "Confirmatory Factor Analysis (CFA) is a theory-driven method used to test whether a predefined latent structure is consistent with the observed data.<br><br>

  In this application, CFA is implemented using the <code>lavaan::cfa()</code> function. A single-factor model is specified in which all items are assumed to load on one common latent variable.<br><br>

  Because the data are ordinal or dichotomous, the model is estimated using robust estimators (ULSMV or WLSMV), and thresholds are modeled instead of continuous intercepts.<br><br>

  The model output provides multiple complementary sources of information:<br><br>

  <b>1. Global model fit indices</b><br>
  These indices evaluate how well the hypothesised factor structure reproduces the observed covariance matrix:<br>
  <ul>
    <li><b>CFI (Comparative Fit Index)</b>: compares the proposed model with a null model (values closer to 1 indicate better fit)</li>
    <li><b>TLI (Tucker-Lewis Index)</b>: similar to CFI but penalises model complexity</li>
    <li><b>RMSEA (Root Mean Square Error of Approximation)</b>: estimates approximate misfit per degree of freedom, including confidence intervals</li>
    <li><b>SRMR (Standardized Root Mean Square Residual)</b>: average standardized residuals between observed and model-implied correlations</li>
    <li><b>Chi-square test</b>: tests exact model fit (sensitive to sample size)</li>
  </ul><br>

  <b>2. Standardized factor loadings</b><br>
  These represent the strength of association between each item and the latent factor. Higher values indicate stronger contributions of items to the construct.<br><br>

  Together, these outputs allow evaluation of whether a reflective measurement model is empirically supported and whether a unidimensional latent structure is plausible for the dataset."
              ),
              br(),
              
              selectInput(
                inputId = "cfa_estimator",
                label = "Desired Estimator:",
                choices = c("ULSMV", "WLSMV"),
                selected = "ULSMV"
                ),
              
              br(),
              
              
              verbatimTextOutput("cfa_output"),
              
              div(
                style = "
    background-color: #FFF8DC;
    border-left: 6px solid #E6B800;
    padding: 15px;
    border-radius: 10px;
    margin-top: 15px;
    margin-bottom: 15px;
    color: #333333;
    font-size: 15px;
    line-height: 1.6;
    box-shadow: 0 2px 5px rgba(0,0,0,0.08);
  ",uiOutput("cfa_interpretation")),
              br(),
              p(HTML("<b>For more information see:</b><br>
                     Bollen, K. A. (1989). <i>Structural equations with latent variables</i>. John Wiley & Sons. <br><br>
                     Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. <i>Structural Equation Modeling, 6</i>, 1–55. https://doi.org/10.1080/10705519909540118 <br><br>
                     Xia, Y., & Yang, Y. (2019). RMSEA, CFI, and TLI in structural equation modeling with ordered categorical data: The story they tell depends on the estimation methods. <i>Behavior Research Methods, 51</i>, 409-428. https://doi.org/10.3758/s13428-018-1055-2"))
            
              ),
            
            # -------------------------------------------
            # IRT
            # -------------------------------------------
            
            tabPanel(
              "Item Response Theory",
              
              h2("Item Response Theory (IRT)"),
              
              br(),
              
              HTML(
                "Item Response Theory (IRT) is a family of probabilistic models used to explain how individuals' responses to items relate to an underlying latent trait.<br><br>

  In this application, IRT is implemented using the <code>eRm</code> package, which estimates Rasch-family models under a conditional maximum likelihood framework.<br><br>

  <b>0. Data preprocessing</b><br>
  Prior to estimation, individuals with insufficient response data (fewer than two valid item responses) are removed to ensure model identifiability and stable estimation.<br><br>

  The specific model used depends on the type of data:<br><br>
  
  <b>1. Dichotomous data (binary items)</b><br>
  When items have two response categories (e.g., 0/1, disagree/agree), the <b>Rasch Model (RM)</b> is estimated using <code>eRm::RM()</code>.<br>
  This model assumes that the probability of endorsing an item increases monotonically with the latent trait, and that all items have equal discrimination.<br><br>

  <b>2. Polytomous data (ordered categories)</b><br>
  When items have more than two ordered response categories, the <b>Rating Scale Model (RSM)</b> is estimated using <code>eRm::RSM()</code>.<br>
  This model extends the Rasch framework by incorporating category thresholds that define transitions between response levels.<br><br>

  The resulting model provides item difficulty estimates, person ability estimates, and fit diagnostics that evaluate how well the Rasch assumptions hold for the observed data."
              ),
              
              verbatimTextOutput("irt_output"),
              
              div(
                style = "
    background-color: #FFF8DC;
    border-left: 6px solid #E6B800;
    padding: 15px;
    border-radius: 10px;
    margin-top: 15px;
    margin-bottom: 15px;
    color: #333333;
    font-size: 15px;
    line-height: 1.6;
    box-shadow: 0 2px 5px rgba(0,0,0,0.08);
  ",uiOutput("irt_interpretation")),
              br(),
              p(HTML("<b>For more information, see:</b><br>
                     Mair, P. (2018). <i>Modern psychometrics with R</i>. Springer International Publishing."))
            )
          )
        ),
        
        # =================================================
        # LATENT MONOTONICITY
        # =================================================
        
        tabPanel(
          "Latent Monotonicity",
          
          h2("Mokken's Monotonicity Test"),

          br(),
          
          p("Monotonicity evaluates whether the probability",
            "of endorsing an item increases as the latent",
            "trait increases."),
          
          p("The following table presents the items, the scalability indices for each item (ItemH
          ), the number of active pairs (ac)—which represents the maximum possible 
            number of tests of monotonicity for each item—, the number of monotonicity 
            violations (Vi) that were identified for each item, the magnitude of the 
            largest violation (MaxVi), the z value of this largest violation (Zmax) 
            for inferential testing, and the number of violations which were significant 
            in each item (Zsig)."
            ),
          
          verbatimTextOutput("monotonicity_output"),
          
          
          div(
            style = "
    background-color: #FFF8DC;
    border-left: 6px solid #E6B800;
    padding: 15px;
    border-radius: 10px;
    margin-top: 15px;
    margin-bottom: 15px;
    color: #333333;
    font-size: 15px;
    line-height: 1.6;
    box-shadow: 0 2px 5px rgba(0,0,0,0.08);
  ",uiOutput("monotonicity_interpretation")),
          
          h2("Parametric Item Response Theory - Item Information"),
          
          div(
            style = "
    background-color:#eef5ff;
    border-left:6px solid #5b9bd5;
    padding:15px;
    border-radius:8px;
    margin-bottom:15px;
    font-size:15px;
    line-height:1.6;
  ",
            
            HTML("
  <b>Visual Assessment of Monotonicity Using Item Characteristic Curves (ICCs)</b><br><br>

  Monotonicity refers to the assumption that the probability of endorsing an item increases as the latent trait increases.<br><br>

  In this application, monotonicity is additionally evaluated through visual inspection of Rasch-family Item Characteristic Curves (ICCs). These curves display how response probabilities change across the latent trait continuum.<br><br>

  <b>Binary (dichotomous) items</b><br>
  For binary items (e.g., 0/1 responses), monotonicity is supported when the probability of endorsing the item increases continuously from low to high trait levels. The ICC should show a smooth increasing S-shaped curve.<br><br>

  <b>Ordinal (polytomous) items</b><br>
  For ordinal items (e.g., Likert scales), monotonicity is evaluated through category response curves. Higher response categories should become progressively more likely at higher levels of the latent trait, with ordered transitions between categories.<br><br>

  <b>Potential indicators of monotonicity problems include:</b>
  <ul>
    <li>Non-increasing or irregular curves</li>
    <li>Disordered category transitions</li>
    <li>Overlapping or poorly separated response categories</li>
    <li>Flat curves with weak discrimination</li>
  </ul>

  These graphical diagnostics complement the nonparametric Mokken monotonicity analysis by providing an intuitive visual evaluation of item functioning along the latent trait continuum.
  ")
          ),
          plotOutput("icc"),
          plotOutput("PI"),
          br(),
          p(HTML("<b>For more information, see:</b><br>
            Franco, V. R., Laros, J. A., & Bastos, R. V. S. (2022). Theoretical and practical foundations of Mokken scale analysis in psychology. <i>Paidéia (Ribeirão Preto), 32 </i>, e3223. https://doi.org/10.1590/1982-4327e3223"
          )),
        ),
        
        
        
        # =================================================
        # LOCAL INDEPENDENCE
        # =================================================
        
        tabPanel(
            "Local Independence",
            
            br(),
            
            h2("Local Independence"),
            
            p(
              "Local independence assumes that item responses ",
              "are conditionally independent given the latent trait."
            ),
            
            h3("Nonparametric Rasch Local Independence Tests"),
            
            tags$b("T1"),
            
            p(
              "Checks for local dependence via increased inter-item correlations. ",
              "For all item pairs, cases are counted with equal responses on both items."
            ),
            
            verbatimTextOutput("local_independence_outputt1"),
            
            tags$b("T11"),
            p("Global test for local dependence. The statistic calculates the ",
                  "sum of absolute deviations between the observed inter-item ",
                  "correlations and the expected correlations."),
              
              verbatimTextOutput("local_independence_outputt11"),
            
            
            h3("Unique Variable Analysis"),
            
            p("Identifies locally dependent (redundant) variables in a multivariate
            dataset using the EBICglasso.qgraph network estimation method and
            weighted topological overlap"),
            
            verbatimTextOutput("local_independence_uva"),
            
            h3("Mokken Local Independence Test"),
            
            p("The function uses three special cases of conditional association
            to identify positive and negative 
              local dependence in the monotone homogeneity model. 
              Straat, Van der Ark, and Sijtsma (2016; also, see Sijtsma, 
              Van der Ark, & Straat, 2015) described the procedure."
            ),
            
            verbatimTextOutput("local_independence_mokken"),
            
            div(
              style = "
    background-color: #FFF8DC;
    border-left: 6px solid #E6B800;
    padding: 15px;
    border-radius: 10px;
    margin-top: 15px;
    margin-bottom: 15px;
    color: #333333;
    font-size: 15px;
    line-height: 1.6;
    box-shadow: 0 2px 5px rgba(0,0,0,0.08);
  ",uiOutput("local_independence_interpretation")),
            br(),
            p(
              HTML(
                "<b>For more information, see:</b> <br>
                
    Christensen, A. P., Garrido, L. E., & Golino, H. (2023). Unique
    variable analysis: A network psychometrics method to detect local 
    dependence. <i>Multivariate Behavioral Research, 58</i>(6), 1165-1182. 
    https://doi.org/10.1080/00273171.2023.2194606<br><br>
    
    Debelak, R., & Koller, I. (2019). Testing the local independence 
    assumption of the Rasch model with Q3-based nonparametric model tests. 
    <i>Applied Psychological Measurement</i>. 
    https://doi.org/10.1177/0146621619835501<br><br>
                
    Straat, J. H., Van der Ark, L. A., &; Sijtsma, K. (2016). 
    Using conditional association to identify locally independent item sets. 
    <i>Methodology, 12</i>(4), 117–123. 
    https://doi.org/10.1027/1614-2241/a000115
    <br><br>
    
    Sijtsma, K., Van der Ark, L. A., & Straat, J. H. (2015). 
    Goodness-of-fit methods for nonparametric IRT models. 
    In L. A. van der Ark, D. M. Bolt, W.-C. Wang, J. Douglas, &; S.-M. Chow (Eds.), 
    <i>Quantitative psychology research: The 79th Annual Meeting of the Psychometric Society, Madison, Wisconsin, 2014</i> 
    (pp. 109–120). Springer. 
    https://doi.org/10.1007/978-3-319-19977-1_9"
              )
            ),
            
            ),
        

div(
  style = "
    background-color:#fff8dc;
    border-left:6px solid #e6b800;
    border-radius:8px;
    padding:12px 16px;
    margin-top:15px;
    margin-bottom:15px;
    font-size:14px;
    line-height:1.6;
    color:#333;
  ",
  
  HTML("
  <b>Note.</b> The interpretations and thresholds presented throughout this application are intended as practical heuristics and general psychometric guidelines to assist interpretation. While these rules of thumb can help identify common patterns of model fit, dimensionality, reliability, and item functioning, psychometric evaluation should always be conducted carefully and within the theoretical and methodological context of each specific dataset and measurement instrument.
  ")
)
)
    )
  )
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
  output$download_report <- downloadHandler(
    
    filename = function() {
      paste0(
        "psychometric_report_",
        Sys.Date(),
        ".html"
      )
    },
    
    content = function(file) {
      
      withProgress(
        message = "Generating psychometric report...",
        value = 0,
        {
          
          # -----------------------------------
          # Step 1 — prepare template
          # -----------------------------------
          
          incProgress(0.2, detail = "Preparing report template...")
          
          tempReport <- normalizePath(
            file.path(tempdir(), "Report.Rmd"),
            winslash = "/",
            mustWork = FALSE
          )
          
          file.copy(
            "Report.Rmd",
            tempReport,
            overwrite = TRUE
          )
          
          # -----------------------------------
          # Step 2 — create parameter list
          # -----------------------------------
          
          incProgress(0.4, detail = "Passing analysis results...")
          
          params <- list(
            data = selected_data()
          )
          
          # -----------------------------------
          # Step 3 — render report
          # -----------------------------------
          
          incProgress(0.7, detail = "Rendering HTML...")
          
          rmarkdown::render(
            input = tempReport,
            output_format = "html_document",
            output_file = basename(file),
            output_dir = dirname(file),
            params = params,
            envir = new.env(parent = globalenv())
          )
          
          # -----------------------------------
          # Finished
          # -----------------------------------
          
          incProgress(1, detail = "Done!")
        }
      )
    }
  )
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
  })
  
  dataset <- reactive({
    
    if (input$data_source == "Example Datasets") {
      
      switch(
        input$example_data,
        
        "Simulated Unidimensional (dichotomous data)" = {
            simdata <- birm::simData(
                          n=1000, 
                          v=10, 
                          l=2, 
                          model="rasch", 
                          seed=666)
            as.data.frame(simdata$data)
        },

        "Big Five Inventory (polytomous data)" = {
          psych::bfi[, c(6:10,1:5,11:25)]}
      )
      
    } else {
      
      req(input$file)
      
      read.csv(
        input$file$datapath,
        header = TRUE
      )
    }
  })
  
  # =================================================
  # VARIABLE SELECTOR
  # =================================================
  
  output$variable_selector <- renderUI({
    
    req(dataset())
    
    selectInput(
      inputId = "selected_vars",
      label = "Choose variables:",
      choices = names(dataset()),
      selected = head(names(dataset()), 5),
      multiple = TRUE
    )
  })
  
  # =================================================
  # SELECTED DATA
  # =================================================
  
  selected_data <- reactive({
    
    req(input$selected_vars)
    
    dataset()[, input$selected_vars, drop = FALSE]
  })
  
  # =================================================
  # CHECK DATA TYPE
  # =================================================
  
  is_binary <- reactive({
    
    dat <- selected_data()
    
    all(
      sapply(dat, function(x) {
        length(unique(na.omit(x))) == 2
      })
    )
  })
  
  is_polytomous <- reactive({
    
    dat <- selected_data()
    
    all(
      sapply(dat, function(x) {
        
        ncat <- length(unique(na.omit(x)))
        
        ncat > 2 && ncat <= 7
      })
    )
  })
  
  # =================================================
  # DESCRIPTIVES
  # =================================================
  
output$selected_table <- renderTable({
    
    desc <- psych::describe(selected_data())
    
    desc$Variable <- rownames(desc)
    
    desc <- desc[, c("Variable", setdiff(names(desc), "Variable"))]
    
    desc
    
  }, rownames = FALSE)


output$correlation <- renderPlot({
  dat <- selected_data()
  
  kendall_results <- corr.test(
    dat,
    method = input$correlationtype,
    use = "pairwise"
  )
  
  corrplot::corrplot(
    kendall_results$r,
    method = "pie",
    type = "upper",
    diag = FALSE,
    number.cex = 1,
    p.mat = kendall_results$p,
    tl.col = "black",
    addCoef.col = "black",
    tl.srt = 45,
    tl.cex = 1,
    col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
    na.label = ' '
  )
  })


output$reliability <- renderUI({
  
  dat <- selected_data()
  req(ncol(dat) >= 3)
  
  # -----------------------------
  # Reliability metrics
  # -----------------------------
  
  alpha <- psych::omega(dat)$alpha
  
  omega_obj <- psych::omega(dat)
  omega <- omega_obj$omega.tot
  
  glb <- tryCatch(
    psych::glb(dat)$glb.Fa
    , error = function(e) NA)
  
  # helper to clamp 0–1
  clamp01 <- function(x) max(0, min(1, x))
  
  make_box <- function(label, value) {
    
    v <- clamp01(value)
    
    col <- grDevices::rgb(
      red   = 1 - v,
      green = v,
      blue  = 1
    )
    
    div(
      style = "width:180px; height:140px;
               border:1px solid #ccc;
               border-radius:10px;
               position:relative;
               overflow:hidden;
               font-size:16px;
               margin-bottom:10px;",
      
      # FULL HEIGHT FILL (bottom-up effect)
      div(
        style = sprintf(
          "position:absolute;
           left:0; bottom:0;
           width:100%%;
           height:%s%%;
           background-color:%s;
           opacity:0.35;",
          v * 100,
          col
        )
      ),
      
      # text overlay
      div(
        style = "position:relative;
                 z-index:2;
                 padding:8px;",
        
        HTML(sprintf(
          "<b>%s</b><br/>Value: %.2f",
          label,
          v
        ))
      )
    )
  }
  
  div(
    style = "display:flex; flex-wrap:wrap; gap:10px;",
    
    make_box("Cronbach's Alpha", alpha),
    make_box("McDonald's Omega", omega),
    make_box("Greatest Lower Bound (GLB)", glb)
  )
})
  # =================================================
  # PARALLEL ANALYSIS
  # =================================================
  
  output$parallel_text <- renderPrint({
    
    dat <- selected_data()

    # ---------------------------------------------
    # Estimate Parallel Analysis
    # ---------------------------------------------
    if (is_binary()|| is_polytomous()) {
            
      EFA.MRFA::parallelMRFA(
        as.matrix(dat),
        Ndatsets = input$pa_iter, 
        percent = 95, 
        corr= "Polychoric",
        display=TRUE,
        graph=FALSE
        )
    }
  }
  )

  
  # =================================================
  # EGA
  # =================================================
  
  output$ega_text <- renderPrint({
    
    dat <- selected_data()
    
    ega_result <- EGAnet::EGA(dat)
    
    print(ega_result)
    
  })
  
  output$ega_plot <- renderPlot({
    
    dat <- selected_data()
    
    dat <- dat[, sapply(dat, is.numeric), drop = FALSE]
    
    req(ncol(dat) >= 3)
    
    ega_result <- EGAnet::EGA(dat)
    
    plot(ega_result)
  })
  
  output$ega_interpretation <- renderUI({
    
    dat <- selected_data()
    
    req(dat)
    
    # keep only numeric
    dat <- dat[, sapply(dat, is.numeric), drop = FALSE]
    
    req(ncol(dat) >= 3)
    
    ega_result <- tryCatch({
      EGAnet::EGA(dat)
    }, error = function(e) NULL)
    
    if (is.null(ega_result)) {
      return(HTML("
      <b>Exploratory Graph Analysis (EGA)</b><br><br>
      The model could not be estimated for the selected dataset.<br>
      This may occur due to insufficient variables, missing data issues, or non-invertible correlation structures.
    "))
    }
    
    # -----------------------------
    # extract number of dimensions
    # -----------------------------
    
    n_dims <- tryCatch({
      length(unique(ega_result$wc))
    }, error = function(e) NA)
    
    n_items <- ncol(dat)
    n_persons <- nrow(dat)
    
    # -----------------------------
    # interpretation logic
    # -----------------------------
    
    dim_interpretation <- if (!is.na(n_dims)) {
      
      if (n_dims == 1) {
        "<b>unidimensional structure</b> (a single dominant latent construct)"
      } else {
        paste0("<b>multidimensional structure</b> (", n_dims, " latent dimensions)")
      }
      
    } else {
      "an unidentified dimensional structure"
    }
    
    complexity_note <- if (!is.na(n_dims) && n_dims > 1) {
      "The presence of multiple dimensions suggests that the item set may reflect distinct but related latent constructs."
    } else {
      "The structure is dominated by a single latent trait, suggesting high internal coherence among items."
    }
    
    # -----------------------------
    # stability info (if available)
    # -----------------------------
    
    stability_note <- tryCatch({
      
      if (!is.null(ega_result$bootEGA)) {
        "Stability analysis (if enabled) can be used to evaluate how consistently items cluster across resampled networks."
      } else {
        "Stability analysis was not computed in this run."
      }
      
    }, error = function(e) "Stability information unavailable.")
    
    # -----------------------------
    # UI output
    # -----------------------------
    
    HTML(sprintf("
  
  <b>Exploratory Graph Analysis (EGA) – Adaptive Interpretation</b><br><br>
  
  <b>1. Data structure</b><br>
  The dataset contains <b>%d items</b> and <b>%d observations</b>.<br>
  EGA is applied to a partial correlation network constructed from these variables.<br><br>
  
  <b>2. Estimated dimensionality</b><br>
  The model identified <b>%s</b>.<br><br>
  
  <b>3. Interpretation of structure</b><br>
  %s<br><br>
  
  <b>4. What indicates a strong structure</b><br>
  - Clearly separated item clusters<br>
  - Strong within-cluster connectivity<br>
  - Weak between-cluster connections<br><br>
  
  <b>5. What may indicate problems</b><br>
  - Weak or unstable clustering<br>
  - Overlapping communities<br>
  - Items switching clusters across solutions<br><br>
  
  <b>6. Integration with other methods</b><br>
  EGA results should be compared with CFA (theory-driven structure), Parallel Analysis (factor retention), and IRT assumptions (unidimensionality checks) to evaluate structural consistency.",
                 
                 n_items,
                 n_persons,
                 dim_interpretation,
                 complexity_note,
                 complexity_note,
                 stability_note
                 
    ))
  })
  # =================================================
  # HULL METHOD
  # =================================================
  output$hull_interpretation <- renderUI({
    
    dat <- selected_data()
    req(ncol(dat) >= 3)
    
    hull <- EFA.MRFA::hullEFA(na.omit(dat), graph = FALSE, display = FALSE)
    
    nfactors <- hull$n_factors
    
    interp <- if (nfactors == 1) {
      
      HTML("
<b>Number of advised dimensions: 1</b><br><br>

This means that the best balance between fit and parsimony is achieved with a <b>unidimensional solution</b>. Although additional factors may slightly improve fit, they do not improve the model enough to justify increased complexity.<br><br>

<b>Practical interpretation:</b>
<ul>
<li>A single dominant latent dimension is sufficient to explain the covariance structure of the items</li>
<li>Additional factors likely reflect minor residual structure rather than meaningful latent constructs</li>
<li>The scale can be interpreted as primarily unidimensional for further psychometric modeling</li>
</ul>
")
      
    } else {
      
      HTML(sprintf("
<b>Number of advised dimensions: %d</b><br><br>

This suggests a <b>multidimensional structure</b>. The Hull Method indicates that multiple latent factors are needed to adequately explain the covariance structure of the items.<br><br>

<b>Practical interpretation:</b>
<ul>
<li>The data likely contain %d meaningful latent dimensions</li>
<li>Items may cluster into distinct subdomains or constructs</li>
<li>Unidimensional models may oversimplify the structure and lead to model misfit</li>
</ul>
", nfactors, nfactors))
    }
    
    interp
  })
  
  output$hull_plot <- renderPlot({
    
    dat <- selected_data()
    
    req(ncol(dat) >= 3)
    
    EFA.MRFA::hullEFA(na.omit(dat))
    
  })
  
  # =================================================
  # CFA
  # =================================================
  output$cfa_output <- renderPrint({
    
    dat <- selected_data()
    dat <- dat[, sapply(dat, is.numeric), drop = FALSE]
    
    req(ncol(dat) >= 3)
    
    model_syntax <- paste0(
      "F1 =~ ",
      paste(colnames(dat), collapse = " + ")
    )
    
    cat("=== Confirmatory Factor Analysis (CFA) ===\n\n")
    
    if (is_binary() || is_polytomous()) {
      
      cat("Estimator:", input$cfa_estimator, "\n\n")
      
      fit <- lavaan::cfa(
        model = model_syntax,
        data = dat,
        ordered = colnames(dat),
        estimator = input$cfa_estimator
      )
      
      # -----------------------------
      # Fit indices
      # -----------------------------
      
      fit_measures <- lavaan::fitMeasures(fit)
      
      cfi   <- fit_measures["cfi.scaled"]
      tli   <- fit_measures["tli.scaled"]
      rmsea <- fit_measures["rmsea.scaled"]
      rmseaupper <- fit_measures["rmsea.ci.upper.scaled"]
      rmseadown <- fit_measures["rmsea.ci.lower.scaled"]
      
      
      srmr  <- fit_measures["srmr"]
      chisq <- fit_measures["chisq.scaled"]
      df    <- fit_measures["df.scaled"]
      pvalue <- fit_measures["pvalue.scaled"]
      
      cat("Model Fit:\n")
      cat(sprintf("  CFI   = %.3f\n", cfi))
      cat(sprintf("  TLI   = %.3f\n", tli))
      cat(sprintf(
        "  RMSEA = %.3f (95%% CI: %.3f – %.3f)\n",
        rmsea, rmseadown, rmseaupper
      ))
      cat(sprintf("  SRMR  = %.3f\n", srmr))
      cat(sprintf("  Chi²  = %.3f (df = %d, p = %f)\n\n", chisq, df, pvalue))
      
      # -----------------------------
      # Standardized loadings
      # -----------------------------
      
      std <- lavaan::standardizedSolution(fit)
      
      loadings_df <- std[std$op == "=~", c("lhs", "rhs", "est.std")]
      
      cat("Standardized Loadings:\n\n")
      
      print(loadings_df, row.names = FALSE)
      
      cat("\n")
      
      cat(sprintf(
        "Summary: range = %.2f – %.2f | loadings < +-.40 = %d items\n\n",
        min(loadings_df$est.std, na.rm = TRUE),
        max(loadings_df$est.std, na.rm = TRUE),
        sum(abs(loadings_df$est.std) < 0.4, na.rm = TRUE)
      ))
      
      weak <- sum(abs(loadings_df$est.std) < 0.4, na.rm = TRUE)
      
      # -----------------------------
      # Interpretation
      # -----------------------------
      
      cat("Interpretation:\n")
      
      if (cfi >= 0.95 && tli >= 0.95 && rmsea <= 0.06 && srmr <= 0.08) {
        
        cat("✔ Good model fit. The hypothesized factor structure is well supported.\n")
        
      } else if (cfi >= 0.90 && tli >= 0.90 && rmsea <= 0.10) {
        
        cat("⚠ Acceptable fit. Model is reasonable but not optimal.\n")
        
      } else {
        
        cat("✖ Poor fit. The model may be misspecified or multidimensional.\n")
      }
      
      if (weak > 0) {
        cat("⚠ Some items show weak loadings (< .40), suggesting weak indicators.\n")
      }
    }
  })
  
  
  output$cfa_interpretation <- renderUI({
    
    dat <- selected_data()
    dat <- dat[, sapply(dat, is.numeric), drop = FALSE]
    req(ncol(dat) >= 3)
    
    model_syntax <- paste0(
      "F1 =~ ",
      paste(colnames(dat), collapse = " + ")
    )
    
    fit <- lavaan::cfa(
      model = model_syntax,
      data = dat,
      ordered = colnames(dat),
      estimator = input$cfa_estimator
    )
    
    fit_measures <- lavaan::fitMeasures(fit)
    
    cfi   <- fit_measures["cfi.scaled"]
    tli   <- fit_measures["tli.scaled"]
    rmsea <- fit_measures["rmsea.scaled"]
    rmseaupper <- fit_measures["rmsea.ci.upper"]
    rmseadown <- fit_measures["rmsea.ci.lower"]
    srmr  <- fit_measures["srmr"]
    chisq <- fit_measures["chisq.scaled"]
    df    <- fit_measures["df.scaled"]
    pvalue <- fit_measures["pvalue.scaled"]
    
    std_loadings <- lavaan::standardizedSolution(fit)
    loadings <- std_loadings$est.std[std_loadings$op == "=~"]
    
    HTML(sprintf("
<b>Confirmatory Factor Analysis (CFA) Model Fit Interpretation</b><br><br>

<b>Global fit indices:</b><br>
<ul>
<li><b>CFI:</b> %.3f (values ≥ .95 indicate excellent fit)</li>
<li><b>TLI:</b> %.3f (values ≥ .95 indicate excellent fit)</li>
<li><b>RMSEA:</b> %.3f (values ≤ .06 indicate good fit)</li>
<li><b>SRMR:</b> %.3f (values ≤ .08 indicate good fit)</li>
<li><b>Chi-square:</b> %.2f (df = %d, p = %.3f)</li>
</ul>

<b>Interpretation of model fit:</b><br>
The combination of these indices indicates how well the hypothesized factor structure reproduces the observed covariance matrix. CFI and TLI assess comparative fit, while RMSEA and SRMR assess absolute misfit.<br><br>

<b>Standardized factor loadings:</b><br>
<ul>
<li>Range: %.2f to %.2f</li>
</ul>

Loadings reflect the strength of the relationship between each item and the latent factor. Values ≥ .50 are generally considered acceptable, and values ≥ .70 indicate strong indicators of the latent construct.<br><br>

<b>Substantive interpretation:</b><br>
If fit indices are within recommended thresholds and most loadings are moderate to high, the hypothesized unidimensional structure is supported. Poor fit or weak loadings suggest model misspecification or multidimensionality.
",
                 cfi, tli, rmsea, srmr,
                 chisq, df, pvalue,
                 min(loadings, na.rm = TRUE),
                 max(loadings, na.rm = TRUE)
    ))
  })
  # =================================================
  # IRT
  # =================================================
  # =================================================
  # IRT MODEL
  # =================================================
    
    irt_model <- reactive({
      
      dat <- selected_data()
      
      dat <- dat[rowSums(!is.na(dat)) > 1, ]
      
      tryCatch({
        
        if (is_binary()) {
          
          eRm::RM(dat)
          
        } else if (is_polytomous()) {
          
          eRm::RSM(dat)
          
        } else {
          
          NULL
        }
        
      }, error = function(e) {
        
        NULL
      })
    })
    # =================================================
    # IRT OUTPUT
    # =================================================
    
    output$irt_output <- renderPrint({
      
      dat <- selected_data()
      
      dat <- dat[rowSums(!is.na(dat)) > 1, ]
      
      cat(
        "Subjects retained after missing-data filtering:",
        nrow(dat),
        "\n\n"
      )
      
      mod <- irt_model()
      
      if (is.null(mod)) {
        
        cat(
          "IRT model could not be estimated.\n"
        )
        
        return()
      }
      
      if (is_binary()) {
        
        cat("Binary data → Rasch Model\n\n")
        
      } else if (is_polytomous()) {
        
        cat("Polytomous data → Rating Scale Model\n\n")
      }
      
      print(mod)
      
      cat("\n")
      
      try({
        
        ppar <- eRm::person.parameter(mod)
        
        print(eRm::itemfit(ppar))
        
      }, silent = TRUE)
    })
  
    output$irt_interpretation <- renderUI({
      
      dat <- selected_data()
      mod <- irt_model()
      
      req(dat)
      
      n_items <- ncol(dat)
      n_persons <- nrow(na.omit(dat))
      
      # safety check
      if (is.null(mod)) {
        return(HTML("
        <b>Item Response Theory (IRT)</b><br><br>
        The model could not be estimated for the selected dataset.<br>
        This usually occurs due to insufficient variability, missing data structure, or violation of Rasch model assumptions.
      "))
      }
      
      # -----------------------------
      # item type
      # -----------------------------
      
      model_type <- if (is_binary()) {
        "Rasch Model (dichotomous items)"
      } else {
        "Rating Scale Model (polytomous items)"
      }
      
      # -----------------------------
      # extract item parameters if possible
      # -----------------------------
      
      eta_info <- tryCatch({
        
        if (!is.null(mod$etapar)) {
          mod$etapar	
        } else {
          NULL
        }
        
      }, error = function(e) NULL)
      
      # spread of difficulties (if available)
      spread_text <- ""
      
      if (!is.null(eta_info)) {
        
        eta_vals <- as.numeric(eta_info)
        
        spread_text <- sprintf(
          "The item difficulty range spans from %.2f to %.2f, indicating %s coverage of the latent trait.",
          min(eta_vals, na.rm = TRUE),
          max(eta_vals, na.rm = TRUE),
          if (diff(range(eta_vals, na.rm = TRUE)) > 2) "broad" else "moderate"
        )
      }
      
      # -----------------------------
      # item-fit summary
      # -----------------------------
      
      fit_summary <- tryCatch({
        
        ppar <- eRm::person.parameter(mod)
        item_fit <- eRm::itemfit(ppar)
        
        infit_mean <- mean(item_fit$i.infitMSQ, na.rm = TRUE)
        outfit_mean <- mean(item_fit$i.outfitMSQ, na.rm = TRUE)
        
        misfit_items <- sum(item_fit$i.infitMSQ > 1.3 | item_fit$i.outfitMSQ > 1.3, na.rm = TRUE)
        
        list(
          infit = infit_mean,
          outfit = outfit_mean,
          misfit = misfit_items
        )
        
      }, error = function(e) NULL)
      
      # -----------------------------
      # dynamic interpretation
      # -----------------------------
      
  HTML(sprintf("
    
    <b>Item Response Theory (IRT) – Adaptive Interpretation</b><br><br>
    
    <b>1. Data structure</b><br>
    The dataset contains <b>%d items</b> and <b>%d valid respondents</b> after missing-data filtering.<br>
    The model estimated is a <b>%s</b>.<br><br>
    
    <b>2. What the model estimates</b><br>
    This model places both items and persons on a shared latent trait continuum.<br>
    Item parameters represent the level of the trait required for endorsement of each item.<br><br>
    
    <b>3. Item difficulty structure</b><br>
    %s<br><br>
    
    <b>4. Model-data fit</b><br>
    Average Infit MSQ: %s<br>
    Average Outfit MSQ: %s<br>
    %s<br><br>
    
    <b>5. What good functioning looks like</b><br>
    - Item fit statistics close to 1.0 indicate good conformity with Rasch expectations<br>
    - Moderate spread in difficulty indicates good measurement coverage across the trait continuum<br>
    - Low number of misfitting items suggests unidimensional structure<br><br>
    
    <b>6. What potential issues would indicate</b><br>
    - Large misfit counts may suggest multidimensionality or local dependence<br>
    - Very narrow difficulty range may indicate limited measurement precision<br>
    - Systematic deviation in fit indices may indicate model misspecification<br><br>
    
    <b>7. Integration with other methods</b><br>
    These results should be interpreted alongside CFA (confirmatory structure), EGA (emergent dimensions), and reliability indices to evaluate whether the scale behaves as a coherent measurement instrument.",
                   
                   n_items,
                   n_persons,
                   model_type,
                   
                   if (spread_text == "") "Item difficulty parameters were estimated on a common latent scale." else spread_text,
                   
                   if (!is.null(fit_summary)) sprintf("%.3f", fit_summary$infit) else "NA",
                   if (!is.null(fit_summary)) sprintf("%.3f", fit_summary$outfit) else "NA",
                   if (!is.null(fit_summary)) paste0("Misfitting items: ", fit_summary$misfit) else "Item-fit summary unavailable"
                   
      ))
    })
  # =================================================
  # ICC PLOT
  # =================================================
  output$icc <- renderPlot({
    
    mod <- irt_model()
    
    req(mod)
    
    tryCatch({
      
      # --------------------------------------------
      # Dichotomous ICC
      # --------------------------------------------
      
      if (is_binary()) {
        
        eRm::plotjointICC(
          mod,
          xlab = "Latent Trait",
          main = "Item Characteristic Curves"
        )
      }
      
      # --------------------------------------------
      # Polytomous ICC
      # --------------------------------------------
      
      else if (is_polytomous()) {
        
        n_items <- ncol(mod$X)
        
        # -----------------------------------------
        # Define layout
        # -----------------------------------------
        
        n_col <- ceiling(sqrt(n_items))
        n_row <- ceiling(n_items / n_col)
        
        old_par <- par(no.readonly = TRUE)
        
        on.exit(par(old_par))
        
        par(
          mfrow = c(n_row, n_col),
          mar = c(3, 3, 2, 1)
        )
        
        # -----------------------------------------
        # Plot all ICCs
        # -----------------------------------------
        
        for (i in seq_len(n_items)) {
          
          eRm::plotICC(
            mod,
            item.subset = i,
            ask = FALSE
          )
        }
      }
      
    }, error = function(e) {
      
      plot.new()
      
      text(
        0.5,
        0.5,
        labels = "ICC plot could not be generated"
      )
    })
  })
  
  # =================================================
  # PERSON-ITEM MAP
  # =================================================
  
  output$PI <- renderPlot({
    
    mod <- irt_model()
    
    req(!is.null(mod))
    
    tryCatch({
      
      eRm::plotPImap(
        mod,
        sorted = TRUE
      )
      
    }, error = function(e) {
      
      plot.new()
      
      text(
        0.5,
        0.5,
        "Person-Item Map could not be estimated"
      )
    })
  })
  # =================================================
  # MONOTONICITY
  # =================================================
  
  output$monotonicity_output <- renderPrint({
    
    dat <- selected_data()
    
    summary(mokken::check.monotonicity(na.omit(dat)))
  })
  
  output$monotonicity_interpretation <- renderUI({
    
    dat <- selected_data()
    req(dat)
    
    # -----------------------------
    # Run analysis safely
    # -----------------------------
    
    result <- tryCatch({
      as.data.frame(summary(mokken::check.monotonicity(na.omit(dat))))
    }, error = function(e) NULL)
    
    # -----------------------------
    # failure case
    # -----------------------------
    
    if (is.null(result) || nrow(result) == 0) {
      return(HTML("
      <b>Mokken Monotonicity Analysis</b><br><br>
      The analysis could not be computed for the selected dataset.<br>
      This usually happens due to low variability, missing data structure, or non-binary/polytomous incompatibility.
    "))
    }
    
    # -----------------------------
    # safe column handling
    # -----------------------------
    
    has_ItemH <- "ItemH" %in% names(result)
    has_zsig  <- "#zsig" %in% names(result)
    
    if (!has_ItemH) {
      return(HTML("
      <b>Mokken Monotonicity Analysis</b><br><br>
      Results were generated but the expected ItemH column was not found.<br>
      This may indicate a version mismatch or unexpected output format.
    "))
    }
    
    # -----------------------------
    # summary metrics (robust)
    # -----------------------------
    
    avg_h <- mean(result$ItemH, na.rm = TRUE)
    
    total_violations <- if (has_zsig) {
      sum(result$`#zsig`, na.rm = TRUE)
    } else {
      NA
    }
    
    n_items <- nrow(result)
    
    # -----------------------------
    # adaptive interpretation logic
    # -----------------------------
    
    structure_interp <- if (avg_h >= 0.5) {
      "strong scalability structure"
    } else if (avg_h >= 0.3) {
      "moderate scalability structure"
    } else if (avg_h >= 0) {
      "weak scalability structure"
    } else {
      "problematic (negative) scalability structure"
    }
    
    violation_interp <- if (is.na(total_violations)) {
      "violation statistics unavailable in this output version"
    } else if (total_violations == 0) {
      "no detected monotonicity violations"
    } else {
      paste0("presence of ", total_violations, " flagged significant violations")
    }
    
    # -----------------------------
    # dataset-sensitive interpretation
    # -----------------------------
    
    context_interp <- if (avg_h > 0.3 && (is.na(total_violations) || total_violations < 5)) {
      "The item set is consistent with a cumulative latent trait, supporting the use of a unidimensional interpretation."
    } else if (avg_h > 0) {
      "The item set shows partial scalability, suggesting that a dominant latent trait may exist but some items may not conform strongly."
    } else {
      "The item set shows weak or inconsistent scalability, which may indicate multidimensionality, poor item quality, or local dependence."
    }
    
    # -----------------------------
    # UI output
    # -----------------------------
    
    HTML(sprintf("

  <b>Mokken Monotonicity Analysis – Adaptive Interpretation</b><br><br>

  <b>1. Purpose of this analysis</b><br>
  This procedure evaluates whether item response probabilities increase monotonically with the latent trait.<br>
  It is a key assumption of nonparametric IRT models such as Mokken scaling.<br><br>

  <b>2. Data overview</b><br>
  The analysis was conducted on <b>%d items</b>.<br>
  The output summarizes scalability (ItemH), violation patterns, and statistical deviation indicators.<br><br>

  <b>3. Scalability (ItemH)</b><br>
  The average scalability coefficient is <b>%.3f</b>, indicating a <b>%s</b>.<br>
  Higher values indicate stronger cumulative ordering of items along a latent trait.<br><br>

  <b>4. Monotonicity violations</b><br>
  %s<br><br>

  <b>5. Interpretation of pattern</b><br>
  %s<br><br>

  <b>6. What this means in practice</b><br>
  - Strong ItemH → items form a reliable cumulative scale<br>
  - Weak ItemH → latent ordering is unstable or noisy<br>
  - Violations → potential local dependence or multidimensionality<br><br>

  <b>7. Integration with other methods</b><br>
  These results should be interpreted alongside CFA (factor structure), EGA (network dimensionality), and IRT (item functioning) to determine whether a coherent latent construct is supported.",
                 
                 n_items,
                 avg_h,
                 structure_interp,
                 violation_interp,
                 context_interp
                 
    ))
  })
  # =================================================
  # LOCAL INDEPENDENCE
  # =================================================

# =================================================
# LOCAL INDEPENDENCE OUTPUTS
# =================================================

output$local_independence_outputt1 <- renderPrint({
  
  dat <- selected_data()
  req(dat)
  
  set.seed(123)
  
  T1 <- tryCatch(
    eRm::NPtest(as.matrix(dat), n = 1000, method = "T1"),
    error = function(e) e
  )
  
  print(T1)
})

output$local_independence_outputt11 <- renderPrint({
  
  dat <- selected_data()
  req(dat)
  
  set.seed(123)
  
  T11 <- tryCatch(
    eRm::NPtest(as.matrix(dat), n = 1000, method = "T11"),
    error = function(e) e
  )
  
  print(T11)
})

output$local_independence_mokken <- renderPrint({
  
  dat <- selected_data()
  req(dat)
  
  print(mokken::check.ca(na.omit(dat), Windex = TRUE))
})

output$local_independence_uva <- renderPrint({
  
  dat <- selected_data()
  req(dat)
  
  print(EGAnet::UVA(dat))
})


# =================================================
# LOCAL INDEPENDENCE INTERPRETATION (FIXED)
# =================================================
output$local_independence_interpretation <- renderUI({
  
  dat <- selected_data()
  req(dat)
  
  n_items <- ncol(dat)
  n_persons <- nrow(dat)
  
  # -----------------------------
  # safe execution
  # -----------------------------
  
  T1 <- tryCatch(
    eRm::NPtest(as.matrix(dat), n = 1000, method = "T1"),
    error = function(e) NULL
  )
  
  T11 <- tryCatch(
    eRm::NPtest(as.matrix(dat), n = 1000, method = "T11"),
    error = function(e) NULL
  )
  
  ca <- tryCatch(
    mokken::check.ca(na.omit(dat), Windex = TRUE),
    error = function(e) NULL
  )
  
  uva <- tryCatch(
    EGAnet::UVA(dat),
    error = function(e) NULL
  )
  
  # -----------------------------
  # UVA redundancy extraction
  # -----------------------------
  
  redundant_pairs <- if (!is.null(uva) && !is.null(uva$redundant)) {
    uva$redundant
  } else {
    NULL
  }
  
  redundant_flag <- !is.null(redundant_pairs) && length(redundant_pairs) > 0
  
  redundant_text <- if (!redundant_flag) {
    "No redundant items detected."
    
  } else {
    paste("Redundancy detected between <br>",
    paste(
      lapply(names(redundant_pairs), function(item) {
        linked <- paste(redundant_pairs[[item]], collapse = ", ")
        paste0("<b>", item, "</b> ↔ ", linked)
      }),
      collapse = "<br>"
    ))
  }

  
  has_T1_issue <- FALSE
  if (!is.null(T1) && !is.null(T1$prop)) {
    has_T1_issue <- any(T1$prop < 0.05, na.rm = TRUE)
  }
  
  has_T11_issue <- FALSE
  if (!is.null(T11) && !is.null(T11$prop)) {
    has_T11_issue <- T11$prop < 0.05
  }
  
  has_ca_issue <- if (is.logical(ca)) {
    as.integer(any(ca, na.rm = TRUE))
  } else {
    as.integer(!isTRUE(ca))
  }
  
  # -----------------------------
  # severity score
  # -----------------------------
  
  violation_level <- sum(
    has_T1_issue,
    has_T11_issue,
    has_ca_issue,
    redundant_flag
  )
  
  overall_state <- if (violation_level == 0) {
    "no evidence of local dependence"
  } else if (violation_level == 1) {
    "minor indications of local dependence"
  } else if (violation_level <= 2) {
    "moderate local dependence"
  } else {
    "substantial violations of local independence"
  }
  
  # -----------------------------
  # UI OUTPUT
  # -----------------------------
  
  HTML(sprintf("
<b>Local Independence – Adaptive Diagnostic Summary</b><br><br>

<b>Items:</b> %d | <b>Respondents:</b> %d<br><br>

<b>Overall conclusion:</b><br>
%s<br><br>

<b>Diagnostics:</b>
<ul>
<li><b>T1:</b> %s</li>
<li><b>T11:</b> %s</li>
<li><b>Mokken Local Independence issue:</b> %s</li>
<li><b>UVA redundancy:</b> %s</li>
</ul><br>

<b>Redundant item structure:</b><br>
%s<br><br>

<b>Interpretation:</b><br>
%s<br><br>

<b>Implication:</b> Local dependence may inflate reliability, distort dimensionality, and bias IRT estimates.",
               
               n_items,
               n_persons,
               
               overall_state,
               
               if (has_T1_issue) "significant dependencies detected" else "not detected",
               if (has_T11_issue) "global violation detected" else "not detected",
               if (has_ca_issue) "violations detected" else "not detected",
               redundant_text,
               
               if (redundant_flag) "redundant items detected" else "none",
               
               
               if (violation_level == 0) {
                 "The assumption of conditional independence appears satisfied."
               } else if (violation_level == 1) {
                 "Mild local dependence is present but might bias results."
               } else if (violation_level <= 2) {
                 "Moderate local dependence suggests possible redundancy or hidden subdimensions."
               } else {
                 "Strong local dependence indicates serious violations of measurement assumptions."
               }
  ))
})
}


# =====================================================
# RUN APP
# =====================================================

shinyApp(ui = ui, server = server)