library(shiny)
library(shinyjs)
library(tidyverse)

richtig_feedback_text <- c("Richtig!",
                           "Genau!",
                           "Klasse!!",
                           "Super!",
                           "Stimmt!",
                           "Perfekt!",
                           "Toll gemacht!",
                           "Fantastisch!",
                           "Prima!",
                           "So ist es!",
                           "Wunderbar!",
                           "Bravo!!",
                           "Supidupi!",
                           "Superkalifragelistik-\nexpialigetisch!!",
                           "Ganz toll!",
                           "Sehr gut!!",
                           "Sehr schön!",
                           "Gut gemacht!",
                           "Hervorragend!",
                           "Spitze!",
                           "Supi!!!")
richtig_feedback_symbol <- c("🎉",
                             "✅", 
                             "✨",
                             "😁",
                             "😃",
                             "😄",
                             "😉",
                             "😍",
                             "😘",
                             "😻",
                             "🚀",
                             "⭐",
                             "🌈",
                             "🌟",
                             "🌸",
                             "🍓",
                             "🍡",
                             "🍩",
                             "🍪",
                             "🍫",
                             "🍬",
                             "🍭",
                             "🍰",
                             "🎁",
                             "🎆",
                             "🎈",
                             "🎊",
                             "🐒",
                             "🐛",
                             "🐠",
                             "🐥")

alleAufgaben <- expand_grid(a = 0:10, b = 0:10)

calc_log_stat <- function(){
  
  log <- read.csv(file = "log.csv") |>
    mutate(aufgabe = paste(a, " ⋅ ", b, " ="),
           zeitstempel = as_datetime(zeitstempel),
           zeitstempel_geklickt = as_datetime(zeitstempel_geklickt),
           dauer = as.numeric(zeitstempel_geklickt - zeitstempel)) |>
    # nur die letzten 5 Antworten jeder Aufgabe berücksichtigen:
    group_by(aufgabe) |>
    mutate(n_Aufgabe = row_number()) |>
    slice_max(n_Aufgabe, n = 5) |> 
    ungroup()
  log_stat <- log |>
    left_join(x = alleAufgaben, y = _, by = c("a", "b")) |>
    summarise(trefferquote = mean(richtig),
              mittl_dauer = median(dauer),
              .by = c(a, b))
}

# UI
ui <- fluidPage(
  useShinyjs(),
 # titlePanel("Kleines 1x1 üben"),
  sidebarLayout(
    sidebarPanel(width = 5,
      h1("Hi Neli!"),
      h1(textOutput("task")),
      br(),
      h3(textOutput("feedback")),
      br(),
      plotOutput("statPlot")
    ),
    mainPanel(width = 7,
      h4("Wähle die richtige Antwort:"),
      fluidRow(
        actionButton("btn_0", "0", style = "width: 50px; height: 50px; margin: 2px; background-color: lightgray;"),
        br()
      ),
      #fluidRow(
        lapply(0:9, function(i) {
          fluidRow(
            lapply(1:10, function(j) {
              num <- i * 10 + j
              actionButton(
                inputId = paste0("btn_", num),
                label = num,
                style = "width: 50px; height: 50px; margin: 2px; background-color: lightgray;"
              )
            })
          )
        })
     # )
    )
  )
)

# Server
server <- function(input, output, session) {

  output$statPlot <- renderPlot({
    task()
    update_plot()
    
    log_stat <- calc_log_stat()
    
    if(all(is.na(log_stat$trefferquote))){return(NA)}
    
    if(all(log_stat$trefferquote == 1, na.rm = TRUE) & all(!is.na(log_stat$trefferquote))){
      random_image_file <- list.files(path = "images/",
                                      pattern = "\\.jpeg$") |>
        sample(size = 1)
      library(jpeg)
      img <- jpeg::readJPEG(source = file.path("images", random_image_file))
      # plot with picture as layer
      ggplot() +
        annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    }else{
    ggplot(log_stat) +
      aes(x = a, y = b, fill = trefferquote, color = mittl_dauer) +
      geom_tile(alpha = 1) +
      scale_fill_gradient2(low = "coral",
                           mid =  "gold", 
                           high = "forestgreen",
                           midpoint = .5,
                           limits = c(0, 1),
                           na.value = "white") +
      geom_point(aes(alpha = mittl_dauer), size = 4) +
      scale_color_gradient(low = "forestgreen", 
                            high =  "pink", 
                           limits = c(0, 50),
                           na.value = "white") +
      theme_void() +
      theme(legend.position = "none")
    }
  })
  
  update_plot <- reactiveVal(Sys.time())
  
  task <- reactiveVal(list(timestamp = lubridate::now(),
                           a = 0, 
                           b = 0, 
                           result = 0))
  
  generateTask <- function() {
    log_stat <- calc_log_stat()
                  
    # Zufallsziehung einer Aufgabe mit Gewichten:
    aufgabe <- log_stat |>
      mutate(trefferquote = replace_na(trefferquote, 0),
             mittl_dauer = replace_na(mittl_dauer, 99)) |>
      ## erstmal vällig zufällige Aufgaben ziehen:
      slice_sample(prop = .42) |>
      ## daraus dann gewichtet nach Dauer (also langsame bevorzugen):
      slice_sample(prop = .5, weight_by = mittl_dauer) |> 
      ## schließlich Ziehung der neuen Aufgabe mit bislang geringster Trefferquote
      slice_min(trefferquote, n = 1, with_ties = FALSE) |> 
      mutate(r = a * b)
    
    task(list(timestamp = lubridate::now(),
              a = aufgabe$a,
              b = aufgabe$b,
              result = aufgabe$r))
  }
  
  generateTask()
  
  output$task <- renderText({
    paste(task()$a, "⋅", task()$b, "= ＿＿＿")
  })
  
  observe({
    lapply(0:100, function(num) {
      observeEvent(input[[paste0("btn_", num)]], {
        ### log Eingabe:
        data.frame(ts_gen = task()$timestamp,
                   a = task()$a,
                   b = task()$b,
                   result = task()$result,
                   clicked = num,
                   ts_clicked = lubridate::now(),
                   task()$result == num) |>
          write.table(file = "log.csv", sep = ",", 
                      append = TRUE, 
                    row.names = FALSE, col.names = FALSE)
        
        btn_id <- paste0("btn_", num)
        if (num == task()$result) {
          output$feedback <- renderText(paste(sample(richtig_feedback_symbol, size = 1),
                                              sample(richtig_feedback_text, size = 1),
                                              sample(richtig_feedback_symbol, size = 1),
                                              collapse = " "))
          runjs(sprintf("$('#%s').css('background-color', 'lightgreen');", btn_id))
          Sys.sleep(1)
          generateTask()
          lapply(0:100, function(n) {
            runjs(sprintf("$('#btn_%s').css('background-color', 'lightgray');", n))
          })
        } else {
          output$feedback <- renderText("Das war nicht richtig. Versuch es nochmal!")
          runjs(sprintf("$('#%s').css('background-color', 'lightcoral');", btn_id))
          Sys.sleep(1)
          runjs(sprintf("$('#%s').css('background-color', 'lightgray');", btn_id))
          update_plot(Sys.time())
        }
      })
    })
  })
}

# App starten
shinyApp(ui, server)
