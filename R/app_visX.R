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

                    # categorical variables
                    conditionalPanel(condition = "input.tabs1=='Categorical variables'",
                                     # bining
                                     h4("Collapsing"),
                                     wellPanel(
                                      uiOutput("vars_bin"),
                                      uiOutput("levels"),
                                      textInput("newcat", "Name of new category"),
                                      selectInput("binned_type", "Type of new variable",
                                                  choices = list("nomial" = "factor",
                                                                 "ordinal" = "ordinal")),
                                      actionButton("cattrans", "Update")),
                                     # Dichotomization?
                                     h4("Dichotomization"),
                                     wellPanel(
                                       uiOutput("vars_bi"),
                                       uiOutput("thres"),
                                       textInput("high_lev", "Name higher level (greater or equal to threshold)"),
                                       textInput("low_lev", "Name lower level (less than threshold)"),
                                       selectInput("bi_type", "Type of new variable",
                                                   choices = list("nomial" = "factor",
                                                                  "ordinal" = "ordinal")),
                                       actionButton("dichot", "Create variable"))),
                    # panel for correlation input
                    conditionalPanel(condition = "input.tabs1=='Network Plot of Correlation'" ,
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
                    width = 2),

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

                                # barplots for categorical variables
                                tabPanel("Categorical variables",
                                         h4("Original variables"),
                                         plotOutput("bar_org", inline = T),
                                         h4("Transformed variables"),
                                         plotOutput("bar_trans", inline = T)),

                                # correlation plot tab
                                tabPanel(title = "Network Plot of Correlation",
                                         plotOutput("npc")),

                                # check
                                tabPanel("Check", textOutput("check"))


                  ))
                  ))
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
  df_lst <- reactiveValues(df_all=NULL, var_type=NULL,
                           df_org=NULL, org_type=NULL,
                           df_new_num=NULL, new_type_num=NULL,
                           df_new_cat=NULL, new_type_cat=NULL,
                           check = NULL)

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
      # fluidRow(
      #   tags$head(
      #     tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: left; vertical-align: middle; } .form-group { display: table-row;}")),
      # column(10,
        selectInput(x, x, choices = list("remove" = "remove",
                                         "numeric" = "numeric",
                                         "nominal" = "factor",
                                         "ordinal" = "ordinal"),
                    selected = types[names==x])})
      })

  # update data tab when initialized
  observeEvent(input$init,
              {names <- bl_df() %>% clean_names(case = "none") %>% colnames()
               opts <- lapply(names, function(x){input[[x]]})
               vars_in <- names[opts!="remove"]
                # remove unselected
                df_lst$df_org = bl_df() %>% clean_names(case = "none") %>%
                  select(all_of(vars_in))
                df_lst$df_all <- df_lst$df_org
                # variable types
                types <- sapply(df_lst$df_org, class)
                types <- unlist(opts[names %in% vars_in])
                df_lst$org_type <- df_lst$var_type <- types
                # display
                output$data = renderDataTable({df_lst$df_org},
                                              options = list(pageLength=10,
                                                            scrollX = T))
                output$datacheck = renderText({data_check(df_lst$df_org)})
                 })

  # numeric variable
   output$vars_dist <- renderUI({
       checkboxGroupInput("vars_dist", "Numeric variables",
                          choices = colnames(df_lst$df_all)[df_lst$var_type=="numeric"])
   })
   ## display original variables
   output$dist_org <- renderPlot({
         make_hist(df_lst$df_org[, df_lst$org_type=="numeric"])},
         height = 600, width = 1000)
   ## transformations: univariate
   observeEvent(input$newtrans,
                {new_vars <- df_lst$df_all %>%
                  mutate(across(all_of(input$vars_dist),
                                .fns = as.formula(paste("~",input$typetrans, "(.x)")),
                                .names = paste("{col}_", input$typetrans, sep = '')),
                         .keep = "none")
               df_lst$df_new_num <- bind_cols(df_lst$df_new_num, new_vars)
               df_lst$new_type_num <- c(df_lst$new_type_num, rep("numeric", ncol(new_vars)))
               df_lst$df_all <- bind_cols(df_lst$df_all, new_vars)
               df_lst$var_type <- c(df_lst$var_type, rep("numeric", ncol(new_vars)))
                              })
   ## transformation: multivariate
   observeEvent(input$newop,
                {if(input$typeop == "Mean"){
                  new_vars = apply(df_lst$df_all[, input$vars_dist], 1, mean)
                }
                 if(input$typeop == "Ratio (alphabetical)"){
                    new_vars = df_lst$df_all[input$vars_dist[1]]/df_lst$df_all[input$vars_dist[2]]
                 }
                  if(input$typeop == "Ratio (reverse alphabetical)"){
                    new_vars = df_lst$df_all[input$vars_dist[2]]/df_lst$df_all[input$vars_dist[1]]
                  }
                  colnames(new_vars) <- input$newvarname
                  df_lst$df_new_num <- bind_cols(df_lst$df_new_num, new_vars)
                  df_lst$new_type_num <- c(df_lst$new_type_num, "numeric")
                  df_lst$df_all <- bind_cols(df_lst$df_all, new_vars)
                  df_lst$var_type <- c(df_lst$var_type, "numeric")

                })
   ## display new variables
   output$dist_trans <- renderPlot({
     if(!is.null(df_lst$df_new_num)){
       make_hist(df_lst$df_new_num)
     }
     else{
       ggplot()+theme_void()+labs(title = "No variables transformed")+
         theme(title = element_text(size = 20))
     }},
     height = 600, width = 1000
     )

   # categorical variable tab
   ## disaplay original variables
   output$bar_org <- renderPlot({
     df_plot <- df_lst$df_org[, df_lst$org_type!="numeric"]
     df_plot <- mutate_all(df_plot, as.character)
     make_bar(df_plot)},
     height = 600, width = 1000)
   ## bining
   output$vars_bin <- renderUI({
     checkboxGroupInput("vars_bin", "Variable to collapse",
                        choices = colnames(df_lst$df_all)[df_lst$var_type!="numeric"])
   })
    output$levels <- renderUI({
      checkboxGroupInput("lev", "Levels to collapse",
                         choices = levels(as.factor(df_lst$df_all[, input$vars_bin])))
    })
    observeEvent(input$cattrans,{
      new_var <- ifelse(df_lst$df_all[ , input$vars_bin] %in% input$lev,
                        input$newcat, df_lst$df_all[ , input$vars_bin])
      new_var <- data.frame(new_var)
      colnames(new_var) <- paste(input$vars_bin, "_bin", sep = "")
      df_lst$df_new_cat <- bind_cols(df_lst$df_new_cat, new_var)
      df_lst$new_type_cat <- c(df_lst$new_type_cat, input$binned_type)
      df_lst$df_all <- bind_cols(df_lst$df_all, new_var)
      df_lst$var_type <- c(df_lst$var_type, input$binned_type)
                  })
   ## dichotomization
   output$vars_bi <- renderUI({
     checkboxGroupInput("vars_bi", "Variable to dichotomize",
                        choices = colnames(df_lst$df_all)[df_lst$var_type=="numeric"])
   })
  output$thres <- renderUI({
      sliderInput("thres_num", "Threshold",
                  min = round(min(df_lst$df_all[, input$vars_bi], na.rm = T), 2),
                  max = round(max(df_lst$df_all[, input$vars_bi], na.rm = T), 2),
                  value = round(min(df_lst$df_all[, input$vars_bi], na.rm = T), 2),
                  round = -1)
  })
  observeEvent(input$dichot,{
    new_var <- ifelse(df_lst$df_all[ , input$vars_bi] <= input$thres_num,
                      input$low_lev, input$high_lev)
    new_var <- data.frame(new_var)
    colnames(new_var) <- paste(input$vars_bi, "_bi", sep = "")
    df_lst$df_new_cat <- bind_cols(df_lst$df_new_cat, new_var)
    df_lst$new_type_cat <- c(df_lst$new_type_cat, input$bi_type)
    df_lst$df_all <- bind_cols(df_lst$df_all, new_var)
    df_lst$var_type <- c(df_lst$var_type, input$bi_type)
  })
  ## display new variables
  output$bar_trans <- renderPlot({
    if(!is.null(df_lst$df_new_cat)){
      make_bar(df_lst$df_new_cat)
    }
    else{
      ggplot()+theme_void()+labs(title = "No variables transformed")+
        theme(title = element_text(size = 20))
    }},
    height = 800, width = 1000
  )

   # correlation diagram panel
   output$vars_cor <- renderUI({
       checkboxGroupInput("vars_cor", "Variables to visualise",
                          choices =  colnames(df_lst$df_all),
                          selected =  colnames(df_lst$df_org))
       })
   observeEvent(input$newvars_cor, {
     df_cor <- df_lst$df_all[, input$vars_cor]
     var_types <- df_lst$var_type[colnames(df_lst$df_all) %in% input$vars_cor]
     cor_mats <- pairwise_cor(df_cor, var_types)
     output$npc <- renderPlot({
       npc_mixed_cor(cor_mats$cor_value, cor_mats$cor_type, cor_mats$cor_p,
                     var_types, show_signif=input$signif!="none",
                     sig.level = input$signif,
                     min_cor = input$min_cor)
            }, height = 600, width = 800)
   })

   # checks
   output$check <- renderPrint({
     df_lst$check})


}


##### App function #####

VisX <- function(){
  shinyApp(ui = ui, server = server, enableBookmarking = "server")
}


#### back up code #####





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
        #

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

   #  ## univariate transformation
   #
   #

   #


   #
   #  # dynamic input panel for categorical variable tab

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



