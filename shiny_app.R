### ------- PH290: Final Project
### authors: Noah Baker, Andrew Nguyen
### desc: a R Shiny app to facilitate easier SQL pulls and visualization of variables

# load packages
pacman::p_load(shiny, bigrquery, DBI, RSQLite, tidyverse, plotly, lubridate, hrbrthemes)

# connect to MIMIC BigQuery database
db <- dbConnect(
  bigrquery::bigquery(),
)


# pull table names from database
data_names <- dbListTables(db)

### ------- front end

ui <- pageWithSidebar(
  headerPanel("MIMIC-III Dataset Visualization"),
  sidebarPanel(
    uiOutput("var_q_1"),
    uiOutput("var_sel_1"),
    uiOutput("var_b_1"),
    uiOutput("output_sel"),
    uiOutput("group_q"),
    uiOutput("group_sel"),
    uiOutput("var_q_2"),
    uiOutput("var_sel_2"),
    uiOutput("make_title"),
    uiOutput("run_query")
  ),
  mainPanel(
    plotlyOutput("plot_out")
  )
)


### ------- back end

server <- function(input, output, session) {
  
  #### render UI ####
  output$var_q_1 <- renderUI({
    radioButtons("sel_q_1",
                 "Number of tables to investigate:",
                 c("1", "2"),
                 selected=1
    )
  })
  
  output$var_sel_1 <- renderUI({
    req(input$sel_q_1)
    selectInput("sel_tbl", #gives first table, needs to dictate other table by shared field
                "Primary table to investigate:",
                data_names
    )
  })
  
  output$var_b_1 <- renderUI({
    req(input$sel_q_1)
    if (isTRUE(input$sel_q_1 == "2")){
      selectInput("sel_tbl_2",
                  "Secondary table to investigate:",
                  second_data_names() #added way to select out table choices based on first table, is reactive
      )
    }
  })
  
  # vis_var is always cat or num
  output$output_sel <- renderUI({
    req(input$sel_tbl)
    if(isTRUE(input$sel_q_1 == 2)){
      req(input$sel_tbl_2)
    }
    selectInput("vis_var",
                "Select a variable to visualize:",
                choices = vis_names()
    )
  })
  
  output$group_q <- renderUI({
    req(input$vis_var)
    radioButtons("group_q_1",
                 "Do you want to GROUP BY?",
                 c("Yes", "No"),
                 selected=character(0)
    )
  })
  
  # group_var is the group by
  output$group_sel <- renderUI({
    req(input$group_q_1)
    if(isTRUE(input$sel_q_1 == 2)){
      req(input$sel_tbl_2)
    }
    if(isTRUE(input$group_q_1 == "Yes")){
      selectInput("group_var",
                  "Select a variable to GROUP BY:",
                  choices = names_groupby()
      )
    }
  })
  
  output$var_q_2 <- renderUI({
    req(input$group_q_1)
    if(isTRUE(input$sel_q_1 == 2)){
      req(input$sel_tbl_2)
    }
    if(isTRUE(input$group_q_1 == "No" && isTRUE(class_visualize() == "integer" | class_visualize() == "numeric"))){
      radioButtons("group_q_2",
                   "Do you want a second variable to visualise?",
                   c("Yes", "No"),
                   selected="No"
      )
    }
  })
  
  # vis_var_2 is always num or date
  output$var_sel_2 <- renderUI({
    req(input$group_q_2)
    if(isTRUE(input$sel_q_1 == 2)){
      req(input$sel_tbl_2)
    }
    if(isTRUE(input$group_q_2 == "Yes" && input$group_q_1 == "No")){
      selectInput("vis_var_2",
                  "Select a second variable to visualize:",
                  vis_names()[vis_names() != input$vis_var]
      )
    }
  })
  
  output$make_title <- renderUI({
    req(input$group_q_1)
    textInput("title", "Give a title for your plot!", value = "Type here!")
  })
  
  output$run_query <- renderUI({
    req(input$group_q_1)
    if(isTRUE(input$sel_q_1 %in% c("1", "2")) && isTRUE(input$group_q_1 %in% c("Yes", "No"))){
      actionButton("go", "Run Query")
    }
  })
  
  
  #### reactives ####
  
  #query to list tables that are joinable by shared variable
  second_data_names <- reactive({
    
    starting_table <- dbListFields(db, input$sel_tbl)
    
    ref_vec <- as.vector(NA)

    for(i in 1:length(data_names)) {
      ref_vec[i] <- as.vector(sum(starting_table %in% DBI::dbListFields(db, data_names[i])) > 0)
    }

    #removes the input table from ouput
    data_names[ref_vec][data_names[ref_vec] != input$sel_tbl]
  })
  
  # small queries to list available variables
  small_query <- reactive({
    if(isTRUE(input$sel_q_1 == "1")){
      dbGetQuery(db, paste0("SELECT * FROM ", input$sel_tbl ," LIMIT 1;"))
    } else {
      dbGetQuery(db, paste0("SELECT * FROM ", input$sel_tbl ," as a JOIN ",
                            input$sel_tbl_2,
                            " as b ON a.hadm_id=b.hadm_id LIMIT 1;"))
    }
  })
  vis_names <- reactive({
    small_query() %>%
      select_if(function(x) is.character(x) | is.numeric(x)) %>%
      names()
  })
  names_groupby <- reactive({
    small_query() %>%
      select_if(function(x) is.character(x) | is.Date(x)) %>%
      names()
  })
  # reactive component for name of column
  name_visualize <- reactive({
    as.character(input$vis_var)
  })
  name_groupby <- reactive({
    as.character(input$group_var)
  })
  # reactive component for class of variable
  class_visualize <- reactive({
    small_query() %>% pull(input$vis_var) %>% class()
  })
  
  
  
  
  #### render plot ####
  
  # erases plot every time a change is made
  observeEvent(ignoreInit = TRUE,
               c(input$sel_q_1, input$sel_tbl, input$sel_tbl_2, input$vis_var,
                 input$group_q_1, input$group_var, input$group_q_2, input$vis_var_2, input$title), {
                   output$plot_out <- renderPlotly({plotly_empty()})
                 })
  
  # makes a plot when the "Run Query" button is pressed
  observeEvent(input$go, {
    output$plot_out <- renderPlotly({
      if(isTRUE(input$go > 0)){
        # isolates to not have to re-query each time changes are made
        sel_var = isolate({
          input$vis_var
        })
        strata = isolate({
          input$group_var
        })
        second_var = isolate({
          input$vis_var_2
        })
        # performs the full SQL pull of the data
        plot_data <- isolate({
          # one table pull only
          if(isTRUE(input$sel_q_1 == "1")){
            # sql pull
            dbGetQuery(db, paste0("SELECT ",
                                  "*",
                                  " FROM ", input$sel_tbl ," LIMIT 10000;"))
          } else{
            dbGetQuery(db, paste0("SELECT ",
                                  "*",
                                  " FROM ", input$sel_tbl,
                                  " as a JOIN ", input$sel_tbl_2,
                                  " as b ON a.hadm_id=b.hadm_id LIMIT 10000;"))
          }
        })
        # if user visualizes categorical variable
        if(isTRUE(class_visualize() == "character")){
          # if they group by
          if(isTRUE(input$group_q_1 == "Yes")){
            # plot categorical by categorical: stacked bar chart
            p <- ggplot(plot_data, aes(x = !!sym(sel_var), fill = !!sym(strata))) +
              geom_bar(position = "stack") +
              labs(title = input$title) +
              hrbrthemes::theme_ipsum() +
              # guides(fill = "none")
              theme(legend.position = "bottom")
          } else {
            # plot one categorical variable: bar chart
            p <- ggplot(plot_data, aes(y = !!sym(sel_var))) +
              geom_bar(stat = "count") +
              labs(title = input$title) +
              hrbrthemes::theme_ipsum()
          }
        }
        # if user visualizes numerical variable
        else {
          # if they group by a categorical variable
          if(isTRUE(input$group_q_1 == "Yes")){
            #plot numeric by categorical: violin/boxplot hybrid
            p <- ggplot(plot_data, aes(x = !!sym(strata), y = !!sym(sel_var), color = !!sym(strata))) +
              geom_boxplot(color="grey", alpha=0.2) +
              geom_violin(fill=NA) +
              labs(title = input$title) +
              hrbrthemes::theme_ipsum() +
              guides(color = "none")
          }
          # they either visualize another numeric variable: scatterplot
          else if(isTRUE(input$group_q_2 == "Yes")) {
            p <- ggplot(plot_data, aes(x = !!sym(sel_var), y = !!sym(second_var))) +
              geom_point() +
              labs(title = input$title) +
              hrbrthemes::theme_ipsum()
            
          }
          # or visualize by itself: histogram
          else {
            p <- ggplot(plot_data, aes(x = !!sym(sel_var))) +
              geom_histogram(aes(y = ..density..)) +
              geom_density() +
              labs(title = input$title) +
              hrbrthemes::theme_ipsum()
          }
        }
        # plot out
        ggplotly(p)
      }
    })
  })
}

#### shiny app ####
shinyApp(ui, server)

