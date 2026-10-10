
library(shiny)
library(readxl)
library(DT)

# Charger les fonctions locales de l'application
fichiers <- list.files(
  "Fonctions",
  pattern = "\\.R$",
  full.names = TRUE
)

invisible(lapply(fichiers, source))

# =========================
# INTERFACE UTILISATEUR
# =========================

ui <- fluidPage(

  titlePanel("epletPairFinder"),

  shinyjs::useShinyjs(),

  sidebarLayout(

    sidebarPanel(

      selectInput(
        "type_analyse",
        "Type d'analyse",
        choices = c(
          "ACC Self/Non-Self" = "classique",
          "ACC Self/Self" = "self"
        ),
        selected = "classique"
      ),

      fileInput(
        "epvix_file",
        "Fichier EpVix",
        accept = c(".xlsx", ".xls")
      ),

      numericInput(
        "mfi",
        "MFI threshold",
        value = 1000,
        min = 0
      ),

      selectInput(
        "eplet_level",
        "Niveau d'eplets",
        choices = c(
          "Tous les eplets" = 1,
          "Confirmed" = 2,
          "Exposed" = 3
        ),
        selected = 1
      ),

      actionButton(
        "analyse",
        "Analyser"
      ),

      br(),
      br(),

      textOutput("message"),

      br(),

      downloadButton(
        "download_excel",
        "Télécharger les résultats Excel"
      )
    ),

    mainPanel(

      tabsetPanel(

        tabPanel(
          "Résultats complets",
          DTOutput("res_total")
        ),

        tabPanel(
          "Résultats probables",
          DTOutput("res_prob")
        )

      )
    )
  )
)

# =========================
# SERVEUR
# =========================

server <- function(input, output, session) {

  # Importer et nettoyer les données EpRegistry
  # Une seule fois par session
  clean_eplet <- import_and_clean_eplets()

  # Stocker les résultats de l'analyse
  resultats <- reactiveVal(NULL)

  # Masquer le choix du niveau d'eplets pour Self/Self
  observe({

    if (input$type_analyse == "self") {
      shinyjs::hide("eplet_level")
    } else {
      shinyjs::show("eplet_level")
    }

  })

  # Lancer l'analyse
  observeEvent(input$analyse, {

    req(input$epvix_file)

    # Effacer les anciens résultats avant la nouvelle analyse
    resultats(NULL)

    output$message <- renderText({
      "Analyse en cours..."
    })

    tryCatch({

      # ACC Self/Self
      if (input$type_analyse == "self") {

        res <- analyse_ACC_self(
          acc_file = input$epvix_file$datapath,
          clean_eplet = clean_eplet,
          seuil = input$mfi,
          soi_file = input$epvix_file$datapath,
          output_excel = FALSE
        )

      } else {

        # ACC Self/Non-Self
        res <- analyse_ACC(
          acc_file = input$epvix_file$datapath,
          clean_eplet = clean_eplet,
          choix = as.numeric(input$eplet_level),
          seuil = input$mfi,
          soi_file = input$epvix_file$datapath,
          output_excel = FALSE
        )

      }

      # Enregistrer les résultats
      resultats(res)

      # Afficher le bilan
      output$message <- renderText({

        req(resultats())

        paste0(
          "Analyse terminée : ",
          if (input$type_analyse == "self") {
            "ACC Self/Self"
          } else {
            "ACC Self/Non-Self"
          },
          "\n\n",
          "Nombre d'eplets du soi : ",
          nrow(resultats()$eplet_soi),
          "\n",
          "Nombre de résultats : ",
          nrow(resultats()$res_total),
          "\n",
          "Nombre de résultats probables : ",
          nrow(resultats()$res_prob)
        )

      })

    }, error = function(e) {

      # Afficher l'erreur sans arrêter l'application
      msg <- conditionMessage(e)

      cat("ERREUR ANALYSE :", msg, "\n")

      output$message <- renderText({
        paste("Erreur pendant l'analyse :", msg)
      })

    })

  })

  # Afficher les résultats complets
  output$res_total <- renderDT({

    req(resultats())

    resultats()$res_total

  })

  # Afficher les résultats probables
  output$res_prob <- renderDT({

    req(resultats())

    resultats()$res_prob

  })


  # Télécharger les résultats dans un fichier Excel
  output$download_excel <- downloadHandler(

    filename = function() {

      nom_analyse <- tools::file_path_sans_ext(
        basename(input$epvix_file$name)
      )

      paste0(
        "Resultats_",
        nom_analyse,
        "_epletPairFinder_",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      )

    },

    content = function(file) {

      req(resultats())

      writexl::write_xlsx(

        list(
          "Resultats complets" = resultats()$res_total,
          "Resultats probables" = resultats()$res_prob,
          "Eplets du soi" = resultats()$eplet_soi
        ),

        path = file

      )

    }

  )

}

# =========================
# LANCEMENT DE L'APPLICATION
# =========================

shinyApp(ui = ui, server = server)
