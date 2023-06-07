
#' @import shiny

ui <- function(request){
  fluidPage(

    # Application title
    titlePanel("Visual Interactive Model Selection"),

    # Sidebar for input
    sidebarLayout(position = "left",
                  # side panel
                  sidebarPanel(
                    # upload file
                    conditionalPanel(condition = "input.tabs1 == 'Data'",
                                     fileInput("bl_df", "Upload file", accept = ".csv"),
                                     uiOutput("initial_vars"),
                                     actionButton("init", "Initialize"),
                                     h5("Note: Re-initialization may override previous operations")),
                    # numeric variables
                    conditionalPanel(condition = "input.tabs1 == 'Numeric variables'",
                                     uiOutput("vars_dist"),
                                     # univariate transformation
                                     h4("Univariate transformation"),
                                     wellPanel(selectInput("typetrans", "Type of transformation",
                                                           choices = c("log", "sqrt")),
                                               actionButton("newtrans", "Transform variable(s)")),
                                     # multivariate transformation
                                     br(),
                                     h4("Multivariate operation"),
                                     wellPanel(selectInput("typeop", "Type of operation",
                                                           choices = c("Ratio (alphabetical)", "Ratio (reverse alphabetical)", "Mean")),
                                               textInput("newvarname", "Name of new variable"),
                                               actionButton("newop", "Create variable(s)"))),

                    # categorical variables
                    conditionalPanel(condition = "input.tabs1=='Categorical variables'",
                                     # bining
                                     h4("Collapsing"),
                                     wellPanel(
                                       uiOutput("vars_bin"),
                                       uiOutput("levels"),
                                       textInput("newcat", "Name of new category"),
                                       radioButtons("binned_type", "Type of new variable",
                                                    choices = list("nominal" = "factor",
                                                                   "ordinal" = "ordinal")),
                                       actionButton("cattrans", "Update")),
                                     # Dichotomization?
                                     h4("Dichotomization"),
                                     wellPanel(
                                       uiOutput("vars_bi"),
                                       uiOutput("thres"),
                                       textInput("high_lev", "Name higher level (greater than threshold)"),
                                       textInput("low_lev", "Name lower level (less than/equal to threshold)"),
                                       radioButtons("bi_type", "Type of new variable",
                                                    choices = list("nominal" = "factor",
                                                                   "ordinal" = "ordinal")),
                                       actionButton("dichot", "Create variable"))),
                    # panel for correlation input
                    conditionalPanel(condition = "input.tabs1=='Network Plot of Correlation and Association'" ,
                                     uiOutput("vars_cor"),
                                     actionButton("newvars_cor", "Update"),
                                     br(),
                                     # correlation threshold
                                     sliderInput("min_cor", "Minimum correlation shown", min = 0, max = 1, value = .3),
                                     # significance
                                     radioButtons("signif", label = "Significance threshold",
                                                  choices = c(0.05, 0.1, "none"),
                                                  selected = "none")
                    ),
                    br(),
                    bookmarkButton(),
                    downloadButton("downloadData", "Download data"),
                    width = 2),

                  # main panel
                  mainPanel(
                    tabsetPanel(id="tabs1",
                                # data
                                tabPanel("Data",
                                         dataTableOutput("data"),
                                         span(htmlOutput("datacheck"),
                                              style="color:red; font-size: 20px")),
                                # histograms for numeric variables
                                tabPanel("Numeric variables",
                                         h4("Original variables"),
                                         plotOutput("dist_org", inline = T),
                                         h4("Transformed variables"),
                                         plotOutput("dist_trans", inline = T)),

                                # barplots for categorical variables
                                tabPanel("Categorical variables",
                                         h4("Original variables"),
                                         plotOutput("bar_org", inline = T),
                                         h4("Transformed variables"),
                                         plotOutput("bar_trans", inline = T)),

                                # correlation plot tab
                                tabPanel(title = "Network Plot of Correlation and Association",
                                         plotOutput("npc")),

                                # correlation matrix tab
                                tabPanel(title = "Correlation and association matrix",
                                         htmlOutput("cormat")),

                                # statistics tab
                                # tabPanel(title = "Statistics",
                                #          htmlOutput("stat")),

                                # check
                                tabPanel(title = "Note",
                                         h4("Supported variable tabs:"),
                                         p("Numeric: continous variable"),
                                         p("Nominal: unorded discrete variable"),
                                         p("Ordinal: ordered discrete varaible"),

                                         h4("Correlation and association:"),
                                         p("Numeric vs Numeric: Spearman correlation and Spearman correlation test"),
                                         p("Nominal vs Numeric/Nominal/Ordinal: PseudoR (square-root of Pseudo R-squared) and p-value from mulitnominal regression"),
                                         p("Ordinal vs  Numeric/Ordinal: GKgamma and GKgamma correlation test"),
                                         p("For correlation test, ****: p<0.0001, ***: p<0.001, **: p<0.01), *:p<0.05)"),

                                         h4("Statistics:"),
                                         h5("R-squared:"),
                                         p("Numeric/Ordinal: R-squared from multiple linear regression"),
                                         p("Nominal: Pseudo R-squared from multinominal regression"),

                                         h4("R session information:"),
                                         verbatimTextOutput("rinfo")
                                )


                    ))
    ))
}



