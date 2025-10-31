tab5_ui <- tabPanel(
  "Help", value = "tab5",
  br(),

  # Factor loading
  h4("What factor loading strength should I specify?"),
  p("If you have a measure with a known reliability estimate",
    "(e.g., Cronbach's alpha), you can estimate the average factor",
    "loading strength of individual items in that measure using the",
    "Spearman-Brown prophecy formula with the calculator below."),

  # Set up Spearman-Brown calculator
  wellPanel(
    # Inputs
    fluidRow(
      column(9,
              sliderInput(
                "alpha",
                label = paste("Reliability estimate of measure",
                              "(e.g., Cronbach's alpha)"),
                min = 0, max = .99, value = .80)),
      column(3,
              numericInput("nitem",
                          label = "Number of items",
                          min = 1, value = 3))
    ),

    # Output
    verbatimTextOutput("lambda.est")
  ),

  # Structural effect size
  h4("What structural effect sizes should I specify?"),
  p("The effect size of a structural parameter (e.g., regression",
    "coefficient between two latent factors) in SEM is often",
    "different from the effect size estimated from regressions using",
    "observed variables, because the structural parameter estimate",
    "could be disattenuated from measurement error. For example, if",
    "prior research found a correlation of .3 between two raw scores,",
    "the effect size of the corresponding true scores is likely larger",
    "(though note this is not necessarily the case if the effect size",
    "from observed variables is estimated in a multivariate path",
    "model; see e.g., Cole & Preacher, 2014). The calculator below",
    "allows you to disattenuate the effect size (in correlation)",
    "between two observed variables using the Spearman's correction."
  ),

  # Set up Spearman's correction calculator
  wellPanel(

    # Inputs
    sliderInput("raw.corr",
                label = "Correlation between observed variables A and B",
                min = 0, max = .99, value = .30),
    fluidRow(
      column(6,
              sliderInput("reliability1",
                          label = "Reliability of variable A",
                          min = 0, max = .99, value = .80)),
      column(6,
              sliderInput("reliability2",
                          label = "Reliability of variable B",
                          min = 0, max = .99, value = .80))),

    # Output
    verbatimTextOutput("latent.corr")
  )
)
