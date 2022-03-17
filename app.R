library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

lapply(list.files(pattern = "*.rda"), load, .GlobalEnv)

ui <- dashboardPage(
  dashboardHeader(title = "Hospital Billing Data"),
  dashboardSidebar(
    numericInput(inputId = "patient_id", label = "Unique ID of a patient", min = 1, max = 14178, value = 100)
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("age"),
      valueBoxOutput("pt_sex"),
      valueBoxOutput("pt_state")
    ),
    fluidRow(
      valueBoxOutput("pt_race"),
      valueBoxOutput("pt_ethnicity"),
      valueBoxOutput("pt_deceased")
    ),
    fluidRow(
      box(
        title = "Billing History Table (Latest First)",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        div(DT::DTOutput("bh_df"), style = "font-size: 80%")
      )
    ),
    fluidRow(
      box(
        title = "Latest Diagnosis",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        div(DT::DTOutput("ld_df"), style = "font-size: 80%")
      )
    ),
    fluidRow(
      verbatimTextOutput("sum")
    )
  )
)

server <- function(input, output) {
  df <- reactive({
    data <- data |>
      filter(ID == input$patient_id) |>
      select(
        ID, age, pt_sex, pt_state, pt_race, pt_ethnicity,
        pt_deceased, dept_name, loc_cat_2_name, new_srvc_date,
        cpt_code, cpt_desc, dx1_code, dx1_desc
      )
    data
  })

  output$age <- renderValueBox(({
    valueBox(df()$age[1], color = "light-blue", "Age")
  }))

  output$pt_sex <- renderValueBox(({
    valueBox(df()$pt_sex[1], color = "light-blue", "Sex")
  }))

  output$pt_state <- renderValueBox(({
    valueBox(df()$pt_state[1], color = "light-blue", "State")
  }))

  output$pt_race <- renderValueBox(({
    valueBox(df()$pt_race[1], color = "light-blue", "Race")
  }))

  output$pt_ethnicity <- renderValueBox(({
    valueBox(df()$pt_ethnicity[1], color = "light-blue", "Ethnicity")
  }))

  output$pt_deceased <- renderValueBox(({
    valueBox(df()$pt_deceased[1], color = "light-blue", "Deceased")
  }))

  output$bh_df <- DT::renderDataTable(df() |> select(dept_name, loc_cat_2_name, cpt_code, cpt_desc, new_srvc_date) |> arrange(desc(new_srvc_date)) |> distinct(), rownames = F, options = list(pageLength = 10))

  output$ld_df <- DT::renderDataTable(df() |> select(dx1_code, dx1_desc) |> distinct(), rownames = F, options = list(pageLength = 10))

  # output$sum <- renderPrint({
  #   df()
  # })
}

shinyApp(ui = ui, server = server)
