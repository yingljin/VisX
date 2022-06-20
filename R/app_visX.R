# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/


#' Run the VisX application
#'
#' @description This function starts the VisX application for basic data pre-processing and variables selection
#' @details This application currently supports files in csv format.
#' @details Application interface will pop up after calling this function;
#' data can be uploaded in that interface;
#' close the interface or interrupt R to stop the application.
#'
#' @return Histograms for continuous variables;
#' @return Barplots for discrete variables;
#' @return Correlation diagram;
#' @return Variance inflation factors (VIFs) and adjusted R-squared;
#' @return Correlation coefficient and p values from correlation test
#'
#' @export
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import here
#' @importFrom janitor clean_names
#' @importFrom fastDummies dummy_cols
#' @importFrom kableExtra kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra row_spec
#' @import knitr
#' @import magrittr
#' @import readr
#' @import stringr
#' @import tibble
#' @importFrom tidyr pivot_longer
#' @examples
#' VisX()
#'
#'

#### UI function ####
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
                                     actionButton("init", "Initialize")),
                    # numeric variables
                    conditionalPanel(condition = "input.tabs1 == 'Numeric variables'",
                                     uiOutput("vars_dist"),
                                    # univariate transformation
                                    h4("Univariate tranformation"),
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


                    br(),
                    bookmarkButton(),
                    width = 3),

                  # main panel
                  mainPanel(
                    tabsetPanel(id="tabs1",
                                # data
                                tabPanel("Data",
                                         dataTableOutput("data"),
                                         span(textOutput("datacheck"),
                                              style="color:red; font-size: 20px")),
                                # histograms for numeric variables
                                tabPanel("Numeric variables",
                                         h4("Original variables"),
                                         plotOutput("dist_org", inline = T),
                                         h4("Transformed variables"),
                                         plotOutput("dist_trans", inline = T)),

                                # check
                                tabPanel("Check", textOutput("check"))
                                ),

                  )))
}




#### server function ####

server <- function(input, output){

  # upload data
  bl_df <- reactive({
    req(input$bl_df)
    ext <- tools::file_ext(input$bl_df$name)
    switch(ext,
           csv = read.csv(input$bl_df$datapath),
           validate("Invalid file; Please upload a .csv file")
    )})

  # list of all reactive datasets
  df_lst <- reactiveValues(df_all=NULL, var_type = NULL, df_trans=NULL)

  # data tab
  output$data <- renderDataTable({bl_df() %>% clean_names(case = "none")},
                                 options = list(pageLength=10,
                                                scrollX = T))
  output$datacheck <- renderText({data_check(bl_df())})
  output$initial_vars <- renderUI({
    names <- bl_df() %>% clean_names(case = "none") %>% colnames()
    types <- sapply(bl_df(), class)
    types <- factor(types,
                    levels = c("numeric", "integer", "factor", "character"),
                    labels = c("numeric", "numeric", "factor", "factor"))
    lapply(names, function(x){
      radioButtons(x, x, choices = c("remove", "numeric", "factor", "ordinal"),
                  inline = T, selected = types[names==x])})
      })

  # update data tab when initialized
  observeEvent(input$init,
              {names <- bl_df() %>% clean_names(case = "none") %>% colnames()
               opts <- lapply(names, function(x){input[[x]]})
               vars_in <- names[opts!="remove"]
                # remove unselected
                df_lst$df_all = bl_df() %>% clean_names(case = "none") %>%
                  select(all_of(vars_in))
                # variable types
                types <- sapply(df_lst$df_all, class)
                types <- unlist(opts[names %in% vars_in])
                df_lst$var_type <- types
                # display
                output$data = renderDataTable({df_lst$df_all},
                                              options = list(pageLength=10,
                                                            scrollX = T))
                output$datacheck = renderText({data_check(df_lst$df_all)})
                output$check = renderPrint(df_lst$var_type)
                 })

  # numeric variable
   output$vars_dist <- renderUI({
       checkboxGroupInput("vars_dist", "Numeric variables",
                          choices = colnames(df_lst$df_all)[df_lst$var_type=="numeric"])
   })
   output$dist_org <- renderPlot({
        # separate original variables and newly created variables
         make_hist(df_lst$df_all[, df_lst$var_type=="numeric"])},
         height = 600, width = 1000)
}


##### App function #####

VisX <- function(){
  shinyApp(ui = ui, server = server, enableBookmarking = "server")
}


#### back up code #####

    #  observeEvent(input$update_init,
    #               {init_df$init_df = bl_df() %>%
    #                 clean_names(case = "none") %>%
    #                 select(all_of(input$initial_selected))})
    #  ### dynamic input panel, reactive to data upload
    #  output$initial_vars <- renderUI({
    #      checkboxGroupInput("initial_selected", "Select variables",
    #                          choices = bl_df() %>% clean_names(case = "none") %>% colnames(),
    #                         selected = bl_df() %>% clean_names(case = "none") %>% colnames())
    #  })
    #  ### dynamic output from initialized data
    #  output$data <- renderDataTable({
    #    if(is.null(init_df$init_df)){
    #        bl_df() %>% clean_names(case = "none")
    #    }
    #    else{init_df$init_df}
    #  })








            # Input panel for transformation of numeric variables
            # conditionalPanel(condition = "input.tabs1=='Numeric variables'",
            #                  uiOutput("vars_dist"),
            #
            #                  # univariate transformation
            #                  h4("Univariate tranformation"),
            #                  wellPanel(
            #                  selectInput("typetrans", "Type of transformation",
            #                              choices = c("log", "sqrt")),
            #                  actionButton("newtrans", "Transform variable(s)")),
            #                  # multivariate transformation
            #                  br(),
            #                  h4("Multivariate operation"),
            #                  wellPanel(
            #                  selectInput("typeop", "Type of operation",
            #                              choices = c("Ratio (alphabetical)", "Ratio (reverse alphabetical)", "Mean")),
            #                  textInput("newvarname", "Name of new variable"),
            #                  actionButton("newop", "Create variable(s)"))),

            # Input panel for collapsing categorical variables
            # conditionalPanel(condition = "input.tabs1=='Categorical variables'",
            #                  #h4("Note: operations in this tab is not reversible", style = "color:red"),
            #
            #                  # bining
            #                  h4("Collapsing"),
            #                  wellPanel(
            #                  uiOutput("vars_bin"),
            #                  uiOutput("levels"),
            #                  textInput("newcat", "Name of new category"),
            #                  actionButton("cattrans", "Update")),
            #
            #                  # Dichotomization?
            #                  h4("Dichotomization"),
            #                  wellPanel(
            #                    uiOutput("vars_bi"),
            #                    uiOutput("thres"),
            #                    textInput("high_lev", "Name higher level (greater or equal to threshold)"),
            #                    textInput("low_lev", "Name lower level (less than threshold)"),
            #                    actionButton("dichot", "Create variable"))),

            # panel for correlation input
            # conditionalPanel(condition = "input.tabs1!='Numeric variables' && input.tabs1!='Data' && input.tabs1!='Categorical variables'" ,
            #                  uiOutput("vars_cor"),
            #                  actionButton("newvars_cor", "Update"),
            #                  br(),
            #                  br(),
            #                  # minimum correlation
            #                  wellPanel(
            #                    selectInput("typecor", "Type of correlation",
            #                                choices = c("spearman", "pearson")),
            #                    # correlation threshold
            #                    sliderInput("min_cor", "Minimum |Correlation| Shown", min = 0, max = 1, value = .3),
            #                    # significance thresohold
            #                    radioButtons("sig", label = "Significance", choices = c(0.05, 0.1, "None"),selected = 0.05),
            #                    # whether or not show all significance or only large correaltion
            #                    checkboxInput("all", "Show all significant association", value = F))),

            # bookmark button


        # # Main panel
        # mainPanel(
        #     tabsetPanel(id="tabs1",
        #                 # data
        #                 tabPanel("Data", dataTableOutput("data")),
                        # distribution
                        # tabPanel("Numeric variables",
                        #          h4("Original variables"),
                        #          plotOutput("dist_org", inline = T),
                        #          h4("Transformed variables"),
                        #          plotOutput("dist_trans", inline = T)),
                        # tabPanel("Categorical variables",
                        #          h4("Original variables"),
                        #          plotOutput("bar_org", inline = T),
                        #          h4("Transformed variables"),
                        #          plotOutput("bar_trans", inline = T)),
                        # correlation plot
                        # tabPanel(title = "Correlation Structure Diagram", plotOutput("networkplot")),
                        # VIF, R2
                        # tabPanel(title = "VisX Information", htmlOutput("varinfo")),
                        # correlation table
                        # tabPanel(title = "Correlation", htmlOutput("corinfo")),
                        # information
                        # tabPanel(title = "Information",
                        #         p("VIF: Variance Inflation Factor; "),
                        #         p("MDE: Marginal Detectible Effect with 80% power (slope);"),
                        #         p("For correlation test, ****: p<0.0001, ***: p<0.001, **: p<0.01), *:p<0.05)"),
                        #         verbatimTextOutput("info")))))



#### Define server logic required to draw a histogram ####
# server <- function(input, output){
#
#     ## data tab: set up data reactive to input
#     ## bl_df is the data uploaded from external files
#     ## init_df is initialized data
#     bl_df <- reactive({
#         req(input$bl_df)
#         ext <- tools::file_ext(input$bl_df$name)
#         switch(ext,
#                csv = read.csv(input$bl_df$datapath),
#                validate("Invalid file; Please upload a .csv file")
#         )})

    # reactive values containing numeric and categorical data separately
    # and list of variable names to be contained on the side bar
    # df_lst <- reactiveValues(df_all = NULL, df_num = NULL, df_cat = NULL)
   #  opt_names
   #

   #
   #  # define a set of reactive values
   #  df_lst <- reactiveValues(df_cat = NULL, df_num = NULL, options = NULL, default = NULL, remove = NULL, new_df_num = NULL, new_default = NULL)
   #
   #  # from data panel: initialization
   # observeEvent(input$update_init,
   #              {df_lst$df_all = bl_df() %>%
   #                clean_names(case = "none") %>%
   #                select(all_of(input$initial_selected))
   #               df_lst$df_all <- df_lst$df_all[, sort(colnames(df_lst$df_all))]
   #               df_lst$df_cat = df_lst$df_all
   #               update_reactive_df(df_lst)
   #               })
   #
   #  # numeric variable panel
   #  ## univariate transformation
   #  observeEvent(input$newtrans,
   #               {df_lst$df_cat = df_lst$df_all %>%
   #                   mutate(across(all_of(input$vars_dist),
   #                                 .fns = as.formula(paste("~",input$typetrans, "(.x)")),
   #                                 .names = paste("{col}_", input$typetrans, sep = '')))
   #               df_lst$remove = sort(c(df_lst$remove, paste(input$vars_dist, "_",input$typetrans, sep = '')))
   #               update_transform_df(df_lst)
   #               })
   #
   #  ## multivariate transformation: only for numeric variables
   #  observeEvent(input$newop,
   #               {if(input$typeop == "Mean"){
   #                 df_lst$df_cat[input$newvarname] = apply(df_lst$df_cat[, input$vars_dist],
   #                                                         1, mean)
   #               }
   #                if(input$typeop == "Ratio (alphabetical)"){
   #                   df_lst$df_cat[input$newvarname] = df_lst$df_cat[input$vars_dist[1]]/df_lst$df_cat[input$vars_dist[2]]
   #                }
   #                 if(input$typeop == "Ratio (reverse alphabetical)"){
   #                   df_lst$df_cat[input$newvarname] = df_lst$df_cat[input$vars_dist[2]]/df_lst$df_cat[input$vars_dist[1]]
   #                 }
   #                 df_lst$remove = sort(c(df_lst$remove, input$newvarname))
   #                 update_transform_df(df_lst)
   #               })
   #
   #  # categorical variable tab
   #  ## binning
   #  observeEvent(input$cattrans,
   #               {new_vars <- paste(input$vars_bin, "_bin", sep = "")
   #                df_lst$df_cat[ , new_vars] = ifelse(df_lst$df_cat[ , input$vars_bin] %in% input$lev, input$newcat, df_lst$df_cat[ , input$vars_bin])
   #                df_lst$remove <- c(df_lst$remove, new_vars)
   #                update_transform_df(df_lst)
   #               })
   #
   #  ## dichotomization
   #  observeEvent(input$dichot,
   #               {new_name <- paste(input$vars_to_dic, "_bi", sep = "")
   #               df_lst$df_cat[ ,  new_name] = ifelse(df_lst$df_cat[ , input$vars_to_dic] >= input$thres_num,
   #                                                    input$high_lev, input$low_lev)
   #               df_lst$df_cat[ , new_name] = as.character(df_lst$df_cat[,  new_name])
   #               df_lst$remove <- c(df_lst$remove, new_name)
   #               update_transform_df(df_lst)
   #               })
   #
   #  # correlation diagram panel
   #  observeEvent(input$newvars_cor,
   #               {if(sum(sapply(df_lst$df_cat, function(x){!is.numeric(x)}))>0){
   #                   df_lst$new_df_num = df_lst$df_cat %>%
   #                       dummy_cols(ignore_na = T, remove_most_frequent_dummy = F) %>%
   #                       clean_names(case = "none") %>%
   #                       select(is.numeric) %>%
   #                       select(all_of(input$vars_cor))}
   #                   else{
   #                       df_lst$new_df_num = df_lst$df_cat %>%
   #                           select(all_of(input$vars_cor))
   #                       }
   #                 df_lst$new_default = input$vars_cor})
   #
   #

   #
   #  # dynamic input panel for categorical variable tab
   #  ## binning
   #  output$vars_bin <- renderUI({
   #    cat_var_names <- df_lst$df_cat %>%
   #      # select(-all_of(df_lst$remove)) %>%
   #      select(!is.numeric) %>%
   #      clean_names(case = "none") %>% colnames()
   #    selectInput("vars_bin", "Variable to collapse", cat_var_names)
   #  })
   #  output$levels <- renderUI({
   #    tryCatch(
   #    checkboxGroupInput("lev", "Levels to collapse", choices = levels(as.factor(df_lst$df_cat[, input$vars_bin]))),
   #    error = function(e){h5("No Categorical variable in this data set")})
   #  })
   #
   #  ## dichotomization
   #  output$vars_bi <- renderUI({
   #    num_var_names <- df_lst$df_cat %>% select(is.numeric) %>%
   #      clean_names(case = "none") %>% colnames()
   #    selectInput("vars_to_dic", "Variable to dichotomize", num_var_names)
   #  })
   #  output$thres <- renderUI({
   #    sliderInput("thres_num", "Threshold", min = round(min(df_lst$df_cat[, input$vars_to_dic], na.rm = T), 2),
   #                max = round(max(df_lst$df_cat[, input$vars_to_dic], na.rm = T), 2),
   #                value = round(min(df_lst$df_cat[, input$vars_to_dic], na.rm = T), 2),
   #                round = -1)
   #  })
   #
   #  # dynamic input panel for correlation tabs
   #  output$vars_cor <- renderUI({
   #    if(is.null(df_lst$new_default)){
   #      default_vars <- df_lst$default
   #    }
   #    else{
   #      default_vars <- df_lst$new_default
   #    }
   #      checkboxGroupInput("vars_cor", "Variables to visualise",
   #                         choices =  df_lst$options,
   #                         selected = default_vars)
   #      })
   #
   #
   # # session info
   #  output$info <- renderPrint({
   #    sessionInfo()
   #      })
   #
   #  # network plot
   #  output$networkplot <- renderPlot({
   #    df <- df_for_figure(df_lst)
   #    tryCatch(network_plot_wsig(df, method = input$typecor, sig.level = ifelse(input$sig!= "None", input$sig, -1),
   #                        overlay = !input$all,
   #                        min_cor = input$min_cor, legend = TRUE, repel = TRUE,
   #                        label_size = 8),
   #             error = function(e){
   #               ggplot()+
   #                 theme_void()+
   #                 labs(title = "Failed to compute correlation")+
   #                 theme(title = element_text(size = 20))})
   #      }, width = 900, height = 900)
   #
   #  # variables info   : vif, r2j
   #  output$varinfo <- renderText({
   #    if(is.null(df_lst$new_df_num)){
   #      df <- df_lst$df_num %>%
   #        mutate(y = rnorm(nrow(df_lst$df_num)))
   #    }
   #    else{
   #      df <- df_lst$new_df_num %>%
   #        mutate(y = rnorm(nrow(df_lst$new_df_num)))
   #    }
   #    vifs <- round(car::vif(lm(y ~ ., data = df)), 1)
   #    r2_j <- round(get_r2j(df %>% dplyr::select(-y)), 2)
   #    cbind("VIF" = vifs,
   #            R2j = r2_j)  %>% as.data.frame() %>%
   #          kable(digits = 2, format = "html") %>%
   #          kable_styling(c("striped", "condensed"), full_width = F)
   #      })
   #
   #  # correlation
   #  output$corinfo <- renderText({
   #    df <- df_for_figure(df_lst)
   #      tb_cor <- corstars(df, method = input$typecor) %>%
   #          rownames_to_column(" ")
   #      # filter out empty column
   #      tb_cor <- tb_cor[, colSums(tb_cor=="")!=nrow(tb_cor)]
   #      # shorten columns names and rownames
   #      new_name <- gsub("_", "\n", colnames(tb_cor))
   #      colnames(tb_cor) <- new_name
   #      #tb_cor[, 1] <- gsub("_", "\n", tb_cor[, 1])
   #      # knit
   #      tb_cor %>%
   #          kable(digits = 2, format = "html", align = "c") %>%
   #          kable_styling(c("striped", "condensed"), full_width = F, font_size = 12) %>%
   #          row_spec(0, extra_css = "transform: rotate(-70deg); height: 100px; text-align: center; vertical-align: middle")
   #      })
   #
   #  # Distribution of numeric variables
   #  output$dist_org <- renderPlot({
   #    # separate original variables and newly created variables
   #     make_hist(init_df$init_df %>% select_if(is.numeric))},
   #    height = 600, width = 1000)
   #
   #  output$dist_trans <- renderPlot({
   #    # separate original variables and newly created variables
   #    tryCatch(make_hist(df_lst$df_cat %>% select_if(is.numeric) %>% select(any_of(df_lst$remove))),
   #    error = function(e){
   #      ggplot()+
   #        theme_void()+
   #        labs(title = "No variables transformed")+
   #        theme(title = element_text(size = 20))})}, height = 600, width = 1000)
   #
   #  # Distribution of categorical variables
   #  output$bar_org <- renderPlot({
   #    tryCatch(make_bar(init_df$init_df %>% select_if(is.character)),
   #             error = function(e){
   #               ggplot()+
   #                 theme_void()+
   #                 labs(title = "No categorical variables in the data set")+
   #                 theme(title = element_text(size = 20))})
   #  }, height = 400, width = 1000)
   #
   #  output$bar_trans<- renderPlot({
   #    tryCatch(make_bar(df_lst$df_cat %>% select_if(is.character) %>% select(any_of(df_lst$remove))),
   #             error = function(e){
   #               ggplot()+
   #                 theme_void()+
   #                 labs(title = "No variables transformed")+
   #                 theme(title = element_text(size = 20))})
   #  }, height = 400, width = 1000)
   #
   #
   #  # bookmark
   #  onBookmark(function(state){
   #    state$values$currentdata <- bl_df
   #    state$values$current_init_df <- init_df$init_df
   #    state$values$currentdf_all <- df_lst$df_all
   #    state$values$currentdf_cat <- df_lst$df_cat
   #    state$values$currentdf_num <- df_lst$df_num
   #    state$values$current_default <- df_lst$default
   #    state$values$current_options <- df_lst$options
   #    state$values$current_remove <- df_lst$remove
   #    state$values$current_new_df_num <- df_lst$new_df_num
   #    state$values$current_new_default <- df_lst$new_default
   #  })
   #
   #  onRestore(function(state){
   #    bl_df <- state$values$currentdata
   #    init_df$init_df <- state$values$current_init_df
   #    df_lst$df_all <- state$values$currentdf_all
   #    df_lst$df_cat <- state$values$currentdf_cat
   #    df_lst$df_num <- state$values$currentdf_num
   #    df_lst$default <- state$values$current_default
   #    df_lst$options <- state$values$current_options
   #    df_lst$remove <- state$values$current_remove
   #    df_lst$new_df_num <- state$values$current_new_df_num
   #    df_lst$new_default <- state$values$current_new_default
   #  })
   #
   #  setBookmarkExclude(c("update_init", "newvars_cor", "newtrans", "newop"))

# }


# Run the application



