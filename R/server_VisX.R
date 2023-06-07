#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @importFrom janitor clean_names
#' @importFrom fastDummies dummy_cols
#' @importFrom kableExtra kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra pack_rows
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra scroll_box
#' @importFrom car vif
#' @import knitr
#' @import readr
#' @import stringr
#' @import tibble
#' @importFrom tidyr pivot_longer
#' @importFrom utils write.csv

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
                           df_new_cat=NULL, new_type_cat=NULL)

  # data tab
  output$data <- renderDataTable({bl_df() %>% clean_names(case = "none")},
                                 options = list(pageLength=10,
                                                scrollX = T))
  output$datacheck <- renderText({data_check(bl_df())})
  output$initial_vars <- renderUI({
    names <- bl_df() %>% clean_names(case = "none") %>% colnames()
    types <- sapply(bl_df(), class)
    types <- factor(types,
                    levels = c("numeric", "integer", "factor", "character", "logical", "NULL"),
                    labels = c("numeric", "numeric", "factor", "factor", "factor", "factor"))
    lapply(names, function(x){
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
    df_plot <- df_lst$df_org[, df_lst$org_type=="numeric"]

    if(nrow(df_plot)>0){
      df_plot <- mutate_all(df_plot, as.numeric)
      make_hist(df_plot)
    }
    else{
      ggplot()+theme_void()+labs(title = "No numeric variables")+
        theme(title = element_text(size = 20))
    }
  },  height = 600, width = 1000)

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
                 new_vars = data.frame(new_vars)
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
    if(any(df_lst$org_type != "numeric")){
      df_plot <- df_lst$df_org[, df_lst$org_type!="numeric",drop = FALSE]
      df_plot <- mutate_all(df_plot, as.character)
      make_bar(df_plot)
    }
    else{
      ggplot()+theme_void()+labs(title = "No nominal variables")+
        theme(title = element_text(size = 20))
    }

   }, height = 600, width = 1000)

  ## binning
  output$vars_bin <- renderUI({
    selectInput("vars_bin", "Variable to collapse",
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
    selectInput("vars_bi", "Variable to dichotomize",
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
    height = 600, width = 1000
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
    # network plot
    output$npc <- renderPlot({
      npc_mixed_cor(cor_mats, show_signif=input$signif!="none",
                    sig.level = input$signif,
                    min_cor = input$min_cor)
    }, height = 800, width = 800)
    # association matrix for display
    format_cor <- corstars(cor_mats$cor_value, cor_mats$cor_p, var_types)
    cor_mat_star <- format_cor$Rnew
    cor_mat_star <- rownames_to_column(cor_mat_star, var = " ")
    row_id <- factor(format_cor$row_id,
                     levels = c("numeric", "factor", "ordinal"),
                     labels = c("numeric", "nominal", "ordinal"))
    row_id <- droplevels(row_id)
    col_id <- as.character(format_cor$col_id)
    col_id <- c(" ", as.character(col_id))
    col_id <- factor(col_id,
                     levels = c(" ", "numeric", "factor", "ordinal"),
                     labels = c(" ", "numeric", "nominal", "ordinal"))
    col_id <- droplevels(col_id)
    output$cormat <-  renderText({
      cor_mat_star %>%
        t() %>%
        kable(escape = F) %>%
        kable_styling("condensed", full_width = F) %>%
        pack_rows(index = table(row_id)) %>%
        add_header_above(table(col_id)) %>%
        scroll_box(width = "100%", height = "1000px")
    })

    # inter-correlation statistics
    df_vif <- mutate(df_cor, y=rnorm(nrow(df_cor)))
    df_vif <- mutate_at(df_vif, which(var_types!="numeric"), as.factor)
    vifs <- round(vif(lm(y ~ ., data = df_vif)), 2)
    r2 <- get_r2(df_cor, var_types)
    r2 <- round(r2, 2)
    var_labs <- factor(var_types, levels = c("numeric", "factor", "ordinal"),
                       labels = c("numeric", "nominal", "ordinal"))
    var_labs <- droplevels(var_labs)
    # output$stat <- renderText({
    #   tb <- data.frame(vifs, r2)
    #   colnames(tb)<- c("GVIF", "DF", "Adjusted GVIF", "R-squared")
    #   tb[order(var_labs), ] %>%
    #     kable(escape = F) %>%
    #     kable_styling(full_width = F) %>%
    #     pack_rows(index = table(var_labs)) %>%
    #     scroll_box(width = "100%", height = "1000px")
    # })
  })

  # session info
  output$rinfo <- renderPrint({sessionInfo()})

  # bookmark
  # bookmark functions saves the current status of reactive values
  # except for conditional values
  onBookmark(function(state){
    state$values$currentdata <- bl_df
    state$values$current_df_all <- df_lst$df_all
    state$values$current_df_org <- df_lst$df_org
    state$values$current_df_new_num <- df_lst$df_new_num
    state$values$current_df_new_cat <- df_lst$df_new_cat
    state$values$current_var_type <- df_lst$var_type
    state$values$current_org_type <- df_lst$org_type
    state$values$current_new_type_num <- df_lst$new_type_num
    state$values$current_new_type_cat <- df_lst$new_type_cat
  })

  onRestore(function(state){
    bl_df <- state$values$currentdata
    df_lst$df_all <- state$values$current_df_all
    df_lst$df_org <- state$values$current_df_org
    df_lst$df_new_num <- state$values$current_df_new_num
    df_lst$df_new_cat <- state$values$current_df_new_cat
    df_lst$var_type <- state$values$current_var_type
    df_lst$org_type <- state$values$current_org_type
    df_lst$new_type_num <- state$values$current_new_type_num
    df_lst$new_type_cat <- state$values$current_new_type_cat
  })

  setBookmarkExclude(c("init", "newvars_cor", "newtrans", "newop", "cattrans", "dichot"))

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("full-VisX-Dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df_lst$df_all, file, row.names = FALSE)
    })
}

