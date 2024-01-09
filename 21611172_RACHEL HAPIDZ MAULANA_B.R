# Load library
library(tidyr)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(rstatix)
library(plotly)

# Sample data
data <- data.frame(
  day = 1:10,
  left = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  center = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  right = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

data2 <- data.frame(
  Ad_Placement = rep(c("Left Sidebar", "Center Page", "Right Sidebar"), each = 10),
  CTR = c(2.5, 3.8, 3.1, 2.7, 3.5, 2.9, 2.8, 4.0, 3.0, 2.6, 3.7, 3.2, 3.0, 3.9, 3.3, 2.4, 3.6, 2.8, 2.9, 4.1, 3.4, 2.5, 3.4, 3.1, 2.6, 3.8, 3.2, 2.7, 3.9, 3.5)
)

data3 <- data.frame( 
  all = c(2.5, 3.8, 3.1, 2.7, 3.5, 2.9, 2.8, 4.0, 3.0, 2.6, 3.7, 3.2, 3.0, 3.9, 3.3, 2.4, 3.6, 2.8, 2.9, 4.1, 3.4, 2.5, 3.4, 3.1, 2.6, 3.8, 3.2, 2.7, 3.9, 3.5)
)
  
# UI
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "CTR Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "input_data"),
      menuItem("Statistical Analysis", tabName = "analisis_statistik"),
      menuItem("Visualization", tabName = "visualisasi")
    ),
    tags$style(
      HTML("
        .main-sidebar {
          background-color: red;
        }
        .main-sidebar a {
          color: white;
        }
      ")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #00fc11;
        }
        .content-wrapper, .right-side {
          background-color: #ebf5ec;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "input_data",
        h2("Input Data"),
        fluidRow(
          column(
            width = 6,
            textInput("left_input", "Input data left", ""),
            textInput("center_input", "Input data center", ""),
            textInput("right_input", "Input data right", ""),
            actionButton("submit_btn", "Add Data")
          ),
          column(
            width = 6,
            DTOutput("data_table"),
            actionButton("edit_data_btn", "Edit Selected Data"),
            actionButton("hapus_data_btn", "Delete Selected Data")
          ),
          column(
            width = 12,
            plotlyOutput("bar_plot")
          )
        )
      ),
      tabItem(
        tabName = "analisis_statistik",
        h2("Statistical Analysis"),
        verbatimTextOutput("output_anova"),
        verbatimTextOutput("anova_output"),
        box(
          title = "LEFT",
          solidHeader = TRUE,
          status = "danger",
          textOutput("box_left")
        ),
        box(
          title = "CENTER",
          solidHeader = TRUE,
          status = "success",
          textOutput("box_center")
        ),
        box(
          title = "RIGHT",
          solidHeader = TRUE,
          status = "warning",
          textOutput("box_right")
        ),
        box(
          title = "ALL",
          solidHeader = TRUE,
          status = "primary",
          textOutput("box_all")
        )
      ),
      tabItem(
        tabName = "visualisasi",
        h2("Visualization"),
        plotlyOutput("bar_plot_visualisasi", height = "600px", width = "100%")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Inisialisasi data default
  rv <- reactiveValues(data = data, data2 = data2, data3 = data3, selected_rows = NULL, editing_row = NULL)
  
  # Menampilkan data tabel
  output$data_table <- renderDT({
    datatable(rv$data, editable = TRUE, selection = "multiple")
  })
  
  # Menambahkan data baru ke tabel
  observeEvent(input$submit_btn, {
    new_row <- data.frame(
      day = nrow(rv$data) + 1,
      left = as.numeric(input$left_input),
      center = as.numeric(input$center_input),
      right = as.numeric(input$right_input)
    )
    rv$data <- rbind(rv$data, new_row)
  })
  
  # Mengatur baris yang dipilih
  observeEvent(input$data_table_rows_selected, {
    rv$selected_rows <- input$data_table_rows_selected
  })
  
  # Hapus data terpilih
  observeEvent(input$hapus_data_btn, {
    if (!is.null(rv$selected_rows)) {
      rv$data <- rv$data[-rv$selected_rows, ]
      rv$selected_rows <- NULL
    }
  })
  
  # Edit data terpilih
  observeEvent(input$edit_data_btn, {
    if (!is.null(rv$selected_rows) && length(rv$selected_rows) == 1) {
      rv$editing_row <- rv$selected_rows
      updateTextInput(session, "left_input", value = as.character(rv$data$left[rv$editing_row]))
      updateTextInput(session, "center_input", value = as.character(rv$data$center[rv$editing_row]))
      updateTextInput(session, "right_input", value = as.character(rv$data$right[rv$editing_row]))
      updateTextInput(session, "all_input", value = as.character(rv$data3[rv$editing_row]))
    }
  })
  
  # Simpan perubahan setelah mengedit
  observeEvent(input$submit_btn, {
    if (!is.null(rv$editing_row)) {
      rv$data$left[rv$editing_row] <- as.numeric(input$left_input)
      rv$data$center[rv$editing_row] <- as.numeric(input$center_input)
      rv$data$right[rv$editing_row] <- as.numeric(input$right_input)
      rv$data3[rv$editing_row] <- as.numeric(input$all_input)
      rv$editing_row <- NULL
    }
  })
  
  # Analisis statistik
  output$output_anova <- renderPrint({
    if (is.null(rv$data)) return(NULL)  # Hindari analisis jika data kosong
    result_anova <- aov(cbind(left, center, right) ~ day, data = rv$data)
    print(summary(result_anova))
    
    # Display Anova test results for data2
    anova_result <- aov(CTR ~ Ad_Placement, data = rv$data2)
    
    output$anova_output <- renderPrint({
      summary(anova_result)
    })
  })
  
  # Menampilkan penjelasan hasil ANOVA
  output$box_left <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(left ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("The p-value (", p_value, ") > 0.05, there is no significant difference for the left sidebar group compared to the other groups")
    } else {
      paste("The p-value (", p_value, ") < 0.05, there is a significant difference for the left sidebar group compared to the other groups.")
    }
  })
  
  output$box_center <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(center ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("The p-value (", p_value, ") > 0.05, there is no significant difference for the center page group compared to the other groups.")
    } else {
      paste("The p-value (", p_value, ") < 0.05, there is a significant difference for the center page group compared to the other groups.")
    }
  })
  
  output$box_right <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(right ~ day, data = rv$data)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("The p-value (", p_value, ") > 0.05, there is no significant difference for the right sidebar group compared to the other groups.")
    } else {
      paste("The p-value (", p_value, ") < 0.05, there is a significant difference for the right sidebar group compared to the other groups.")
    }
  })
  
  output$box_all <- renderText({
    if (is.null(rv$data)) return(NULL)
    result_anova <- aov(CTR ~ Ad_Placement, data = rv$data2)
    p_value <- format(summary(result_anova)[[1]]$'Pr(>F)'[1], digits = 3)
    if (as.numeric(p_value) > 0.05) {
      paste("The p-value (", p_value, ") > 0.05, there is no significant difference for all groups.")
    } else {
      paste("The p-value (", p_value, ") < 0.05, there is a significant difference for all groups.")
    }
  })
  # Visualisasi diagram batang
  output$bar_plot_visualisasi <- renderPlotly({
    if (is.null(rv$data)) return(NULL)
    
    # Data untuk plot
    data_plot <- rv$data %>%
      pivot_longer(cols = c(left, center, right), names_to = "Group", values_to = "Value")
    
    # Define colors for the pie chart slices
    slice_colors <- c("ff5733",	"ffbd33",	"ff3375")  # Replace these colors with your preferred ones
    
    # Plot pie chart
    p <- plot_ly(data_plot, labels = ~Group, values = ~Value, type = "pie") %>%
      layout(title = "Number of Clicks Based on Ad Placement", font = list(size = 24), margin = list(t = 100), showlegend = TRUE)%>%
      add_trace(marker = list(colors = slice_colors))
    
    return(p)
  })
}

# Run the application
shinyApp(ui, server)