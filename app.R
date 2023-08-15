source("utils.R")
source("global.R")


ui <- dashboardPage(
  # this section is to set up the overall dashboard structure + format
  
  # CAN SELECT THE THEME IN THE APP
  controlbar = dashboardControlbar(skin="dark", selected = "blue", collapsed = TRUE, skinSelector()),
  skin="blue",
  dashboardHeader(
    title = "CCDF Calculator",
    titleWidth = 200 # this is a pixel count and is not dynamic
    ), # close header
  
  # SIDEBAR OPTIONS
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("About CCDF & The Project", tabName = "about_tab", icon = icon("book")),
      menuItem("Calculator", tabName = "plots_tab", icon = icon("dashboard")),
      menuItem("Tables", tabName = "tables_tab", icon = icon("tablet"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # the section above to set up the overall dashboard structure + format
      # The bulk of the dashboard code
      
      # About tab
      tabItem(
        tabName = "about_tab",
        titlePanel("About CCDF & Project"),
        h3("Problem"),
        p("The Child Care and Development Fund (CCDF) is a $5 billion public program aiming to provide financial aid in order to facilitate child care for low-income households. While the CCDF program currently assists over 1.4 million children in the US (What Is the Child Care and Development Fund (CCDF)?, 2016), there is a lot of uncertainty, inequality, and administrative burden associated with the process (Jenkins & Nguyen, 2021). Furthermore, the eligibility criteria can be unclear, especially for minority populations (e.x., Hill et al., 2019). Thus, the goal of this project is to raise awareness of the program and inform families of their eligibility based on past data. "),
        div(),
        h3("Audience"),
        p("The main audience of the dashboard is low-income families with children, potentially qualifying for the aid. This project provides them with a calculator tool to review how many similar families have received aid over the years. Apart from them, the website can be of interest to policymakers and researchers."),
        div(),
        h3("Statistics"),
        fluidRow( 
          valueBoxOutput("count_fam"),
          valueBoxOutput("count_child"),
          valueBoxOutput("mean_payment"),
        ),
        h3("Average Payment Amount in Your State"),
        
        fluidRow(
          # main diagram
          box(
            title = "Select Your State",
            status = "primary",
            width = 6,
            selectInput(inputId = "state_input2",
                        label = "Select State",
                        choices = STATE_vars2)
          ),
          box(
            title = "Average Payment Amount in Your State",
            # width = 9,
            plotOutput("plot_payment"))
        ), #2nd fluidrow ends
        h3("About Data"),
        p("Data for the project will be taken from the University of Michigan Institution for Social Research website (Child Care and Development Fund (CCDF) Administrative Data Series, 2023). The source contains data on families and children receiving assistance through the CCDF from 2001 to 2019 in each US state. Data is accompanied by a code book clearly interpreting the variable names."),
        h3("References"),
        p("Child Care and Development Fund (CCDF) Administrative Data Series. (2023). https://www.icpsr.umich.edu/web/ICPSR/series/215"),
        p("Jenkins, J. M., & Nguyen, T. (2021). Keeping Kids in Care: Reducing Administrative Burden in State Child Care Development Fund Policy. Journal of Public Administration Research and Theory, 32(1), 23–40. https://doi.org/10.1093/jopart/muab020"),
        p("Hill, Z., Gennetian, L. A., & Mendez, J. L. (2019). A descriptive profile of state Child Care and Development Fund policies in states with high populations of low-income Hispanic children. Early Childhood Research Quarterly. https://doi.org/10.1016/j.ecresq.2018.10.003"),
        p("What is the Child Care and Development Fund (CCDF)? (2016, December 14). The Administration for Children and Families. https://www.acf.hhs.gov/archive/occ/faq/what-child-care-and-development-fund-ccdf")
      ),
      # Close About tab
      
      # Plots Tab
      tabItem(
        tabName = "plots_tab",
        titlePanel("Calculator"),
        h3("Use the dropdown menu below to choose options that best describe your family."),
        p("This will allow you to view summary statistics and a plot showcasing how many similar families have benefited from the program over the years."),
        fluidRow( 
          valueBoxOutput("perc_fam"),
          valueBoxOutput("mean_income"),
          valueBoxOutput("mean_fam"),
        ),
        fluidRow(
          # main diagram
          box(
            title = "Select Your Family Characteristics", 
            status = "primary", 
            width = 6,
            selectInput(inputId = "state_input", 
                        label = "Select State", 
                        choices = STATE_vars, 
                        selected = "AZ"),
            selectInput(inputId = "singpar_input", 
                        label = "Are you a single parent?", 
                        choices = SINGPAR_vars),
            selectInput(inputId = "incemp_input", 
                        label = "Is employment included as your source of income?", 
                        choices = INCEMP_vars),
            selectInput(inputId = "INCFDST_input", 
                        label = "Do you receive Assistance under the Supplemental Nutrition Assistance Program, formally the Food Stamp Act ?", 
                        choices = INCFDST_vars),
            numericInput(inputId = "INCOME_input", 
                         label = "What is your family monthly income", 
                         value = 2000,
                         min = 0,
                         max = max(INCOME_vars)),
            numericInput(inputId = "FAMILYSZ_input", 
                         label = "Number of family members", 
                         value = 10,
                         min = min(FAMILYSZ_vars),
                         max = max(FAMILYSZ_vars))
            ),
        box(
          title = "Number of families with similar characteristics (2014-2019)", 
          # width = 9,
          plotOutput("plot"))
          ) #2nd fluidrow ends
      ),
      # Close Plots Tab
      

      # Table Start Tabs
      tabItem(
        tabName = "tables_tab",
        # Application title
        titlePanel("Tables"),
        fluidRow(
          box(
            title = "Download Similar Families",
            width = 3,
            downloadButton("download_table", "Download")
            ),
          box(title = "Table of similar families in the chosen state 2014-2019",
              width = 9,
              dataTableOutput("table_filtered"))
        ),
        fluidRow(
          box(
            title = "Download the Entire State Data",
            width = 3,
            downloadButton("download_table_state", "Download")
          ),
          box(title = "Table for chosen state 2014-2019",
              width = 9,
              dataTableOutput("table_state"))
        ), 
      ) # Close table tab
     ) # Close tabs in general
   ) # Close dashboardbody
) # Close dashboardpage

server <- function(input, output, session) {
  
  # REACTIVE TABLES
  
  # filtered data
  uploaded_data <- reactive(data %>% 
                              filter(STATE == input$state_input,
                                     SINGPAR == input$singpar_input,
                                     INCEMP == input$incemp_input,
                                     INCFDST == input$INCFDST_input,
                                     INCOME <= input$INCOME_input,
                                     FAMILYSZ <= input$FAMILYSZ_input))
  # state family data 
  state_data = reactive(data %>% 
                          filter(
                            STATE == input$state_input
                          ))
   
  # state payments data
  state_data_payment = reactive(data2 %>% 
                           filter(
                             STATE == input$state_input2
                           ))
   
  # render plot for families
  output$plot <- renderPlot({
    make_plot(uploaded_data())
  }, res = 96)
  
  # render plot for payments
  output$plot_payment <- renderPlot({
    make_plot_payment(state_data_payment())
  }, res = 96)
  
  # GET THE NAME OF THE STATE
  output$state_name <- renderText({
    input$state_input
  })
  
  ## BOXES
  
  # COUNT FAMILIES
  output$count_fam <- renderValueBox({
    valueBox(
      count(data), "Number of families that received aid in the years 2014-2019",
      icon = icon("bank"),
      color = "blue"
    )
  })
  
  # COUNT CHILDREN
  output$count_child <- renderValueBox({
    valueBox(
      count(data2), "Number of children enrolled in the program in 2014-2019",
      icon = icon("bank"),
      color = "blue"
    )
  })
  
  # MEAN PAYMENT
  output$mean_payment <- renderValueBox({
    valueBox(
      round(mean(data2$PAYMENT),2), "Average Payment (in US dollars) that Families received in 2014-2019",
      icon = icon("bank"),
      color = "blue"
    )
  })
  
  # INCOME: PERCENTAGE OF SIMILAR FAMILIES IN THE STATE
  output$perc_fam <- renderValueBox({
    caption = str_c("Percentage of similar families in ", input$state_input," state getting the aid in 2014-2019")
    valueBox(
      round(perc(count(uploaded_data()), count(state_data())), 2), caption,
      icon = icon("bank"),
      color = "blue"
    )
  })
  
  # MEAN INCOME FOR THE STATE
  output$mean_income <- renderValueBox({
    caption = str_c("Average monthly income of families receiving aid in ", input$state_input," state in 2014-2019")
    valueBox(
      round(mean_income(state_data()), 2), caption,
      icon = icon("bank"),
      color = "blue"
    )
  })  
  
  
  # AVERAGE FAMILY SIZE
  output$mean_fam <- renderValueBox({
    caption = str_c("Average number of family members upon which eligibility is based in ", input$state_input," state in 2014-2019")
    valueBox(
      round(mean_fam(state_data()), 2), caption,
      icon = icon("bank"),
      color = "blue"
    )
  })  
  
  # Tables
  
  # TABLE FILTERED
  output$table_filtered <- renderDataTable(uploaded_data())
  
  # TABLE STATE
  output$table_state <- renderDataTable(state_data())
  
  # DOWNLOAD BUTTON - similar families
  output$download_table <- downloadHandler(
    filename = function() {
      paste("CCDF_similar_families.csv")
    }, #the function call is odd, but required
    content = function(file) { #the file var is odd, but required
      write.csv(uploaded_data(), file, row.names = FALSE)
    }
  )
  
  # DOWNLOAD BUTTON - state
  output$download_table_state <- downloadHandler(
    filename = function() {
      paste("CCDF_state.csv")
    }, #the function call is odd, but required
    content = function(file) { #the file var is odd, but required
      write.csv(state_data(), file, row.names = FALSE)
    }
  )
  
} # end of server

shinyApp(ui, server)