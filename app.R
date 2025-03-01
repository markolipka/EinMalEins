library(shiny)
library(shinyjs)
library(tidyverse)
library(babynames)
library(jpeg)

pseudoname <- babynames %>% slice_sample(n = 1) %>% pull(name)

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
                           "Superkalifragelistikexpialigetisch!!",
                           "Ganz toll!",
                           "Sehr gut!!",
                           "Sehr schön!",
                           "Gut gemacht!",
                           "Hervorragend!",
                           "Spitze!",
                           "Supi!!!")
richtig_feedback_symbol <- c("🎉", "✅", "✨",
                             "😁", "😃", "😄", "😉","😍", "😘", "😻",
                             "🚀", "⭐", "🌈", "🌟", "🌸", "🍓", "🍡",
                             "🍩", "🍪", "🍫", "🍬", "🍭", "🍰", "🎁",
                             "🎆", "🎈", "🎊", "🐒", "🐛", "🐠", "🐥")

alleAufgaben <- expand_grid(a = 0:10, b = 0:10)

# UI
ui <- fillPage(
  useShinyjs(),
 # titlePanel("Kleines 1x1 üben"),
 # Zentrieren der gesamten Seite
 div(style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100vh;", 
     
  inputPanel(
    actionButton("newTask", label = "Eine neue Aufgabe!"),
    h1(textOutput("task")),
  ),
    h2(textOutput("feedback")),
  fillRow(
    uiOutput("dynamicUI", fill = TRUE)
  )
 )
)

# Server
server <- function(input, output, session) {
  
  name <- reactiveVal(NULL)  # Speichert den eingegebenen Namen
  
  # Zeige den Dialog nur beim Start
  observeEvent(TRUE, {
    showModal(modalDialog(
      title = "Hi! Wer bist du?",
      textInput("benutzerinnenname", label = NULL, value = pseudoname),
      footer = modalButton("OK"),
      size = "l"
    ))
    
    update_stats(Sys.time())
    output$dynamicUI <- renderUI({
      plotOutput("statPlot", height = "100vh") # volle Screen-Höhe ausnutzen
    })
  }, once = TRUE)  # `once = TRUE`, damit die Abfrage nur am Anfang kommt
  
  
  
  output$buttonGrid <- renderUI({
    fillPage(
      div(style = "text-align: center;", 
      h4("Wähle die richtige Antwort:"),
          fluidRow(
            actionButton("btn_0", "0",
                         style = "width: 50px; height: 50px; margin: 2px; background-color: lightgray;")
          ),
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
      )
    )
  })
  
  log_stat <- reactive({
    update_stats()
    
    log <- read.csv(file = "log.csv") |>
      filter(benutzerinnenname == input$benutzerinnenname) |>
      mutate(aufgabe = paste(a, " ⋅ ", b, " ="),
             zeitstempel = as_datetime(zeitstempel),
             zeitstempel_geklickt = as_datetime(zeitstempel_geklickt),
             dauer = as.numeric(zeitstempel_geklickt - zeitstempel)) |>
      # nur die letzten 5 Antworten jeder Aufgabe berücksichtigen:
      group_by(aufgabe) |>
      mutate(n_Aufgabe = row_number()) |>
      slice_max(n_Aufgabe, n = 5) |> 
      ungroup()
    log |>
      left_join(x = alleAufgaben, y = _, by = c("a", "b")) |>
      summarise(trefferquote = mean(richtig),
                mittl_dauer = median(dauer),
                .by = c(a, b))
  })
  
  observeEvent(input$newTask, {
    
    naechstes_huebsches_bild(sample(list.files("images/", pattern = "\\.jpeg$"), 1))
    
    # Zufallsziehung einer Aufgabe mit Gewichten:
    aufgabe <- log_stat() |>
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
    
    output$task <- renderText({
      paste(task()$a, "⋅", task()$b, "= ＿")
    })
    
    output$feedback <- renderText("😊")
    
    output$dynamicUI <- renderUI({
      uiOutput("buttonGrid")
    })
    
    # lapply(0:100, function(n) {
    #   runjs(sprintf("$('#btn_%s').css('background-color', 'lightgray');", n))
    # })
  })
  
  update_stats <- reactiveVal(Sys.time())
  
  
  task <- reactiveVal(list(timestamp = lubridate::now(),
                           a = 0, 
                           b = 0, 
                           result = 0)
                      )
  
  naechstes_huebsches_bild <- reactiveVal(sample(list.files("images/", pattern = "\\.jpeg$"), 1))
  

  # Status-Plot generieren:
  output$statPlot <- renderPlot({
   
    log_stat <- log_stat()
    
    ## leere Plot, wenn noch keine Ergebnisse im Log:
   # if(all(is.na(log_stat$trefferquote))){return(NA)}
    
    ## Hübsches Bild, falls Ziel-Trefferquote erreicht: 
    if(all(log_stat$trefferquote == 1, na.rm = TRUE) & 
       all(!is.na(log_stat$trefferquote))){
      
      # Verwende das bereits vorbereitete Bild
      img <- readJPEG(file.path("images", naechstes_huebsches_bild()))
      
      # plot with picture as layer
      ggplot() +
        annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        coord_fixed(ratio = 1)
    }else{
      ## Ansonsten die Status-Matrix:
      ggplot(log_stat) +
        aes(x = a, y = b, fill = trefferquote) +
        geom_tile(alpha = .8) +
        scale_fill_gradient2(low = "darkred",
                             mid =  "gold", 
                             high = "forestgreen",
                             midpoint = .5,
                             limits = c(0, 1),
                             na.value = "snow") +
        geom_text(aes(alpha = mittl_dauer), 
                   size = 15, label = "\u23f1", family = "Lucida Sans Unicode") +
        scale_alpha_continuous(limits = c(3, 10), range = c(0, 1),
                               oob = scales::squish, 
                               na.value = 0) +
        theme_void() +
        theme(legend.position = "none", 
              panel.border = element_rect(colour = "forestgreen", 
                                          fill = NA, 
                                          linewidth = 1)
              ) +
        #ggtitle(paste("So weit ist", input$benutzerinnenname, "schon:")) +
        coord_fixed(ratio = 1)
    }
  })
  
  observe({
    lapply(0:100, function(num) {
      observeEvent(input[[paste0("btn_", num)]], {
        ### log Eingabe:
        data.frame(benutzerinnenname = input$benutzerinnenname,
                   ts_gen = task()$timestamp,
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
        if (num == task()$result) { # fall richtiges Ergebnis geklickt:
          ## zufälliges, positives Feedback wählen:
          output$feedback <- renderText(paste(sample(richtig_feedback_symbol, size = 1),
                                              sample(richtig_feedback_text, size = 1),
                                              sample(richtig_feedback_symbol, size = 1),
                                              collapse = " "))
          ## geklickten Button grün färben:
         # runjs(sprintf("$('#%s').css('background-color', 'lightgreen');", btn_id))
          
          ## Angezeigte Aufgabe mit korrektem Ergebnis anreichern:
          output$task <- renderText({
            paste(task()$a, "⋅", task()$b, "= ", num)
          })
          
          update_stats(Sys.time())
          output$dynamicUI <- renderUI({
              plotOutput("statPlot", height = "60vh") # volle Screen-Höhe ausnutzen
          })
          
        } else { # falls ein falsches Ergebnis geklickt:
          output$feedback <- renderText("Das war nicht richtig. Versuch es nochmal!")
          runjs(sprintf("$('#%s').css('background-color', 'lightcoral');", btn_id))
          Sys.sleep(1)
          runjs(sprintf("$('#%s').css('background-color', 'lightgray');", btn_id))
          
          output$dynamicUI <- renderUI({
            uiOutput("buttonGrid")
          })
        }
      })
    })
  })
}

# App starten
shinyApp(ui, server)
