library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
library(rsconnect)
library(shinylive)

# Chargement des bases de données + correction de celles-ci

arsenal24_25 <- read_csv("arsenal24-25.csv")
arsenal_joueuses <- read_csv("arsenal - Joueuse.csv")

names(arsenal24_25) <- trimws(names(arsenal24_25))
names(arsenal_joueuses) <- trimws(names(arsenal_joueuses))

arsenal24_25 <- arsenal24_25 %>%
  mutate(across(where(is.character), trimws))

arsenal_joueuses <- arsenal_joueuses %>%
  mutate(across(where(is.character), trimws))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Arsenal Women 2024-2025"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue d'ensemble", tabName = "equipe", icon = icon("home")),
      menuItem("Statistiques", tabName = "statistiques", icon = icon("chart-bar")), 
      menuItem("Joueuses", tabName = "joueuses", icon = icon("users")),
      menuItem("Matchs", tabName = "matchs", icon = icon("futbol"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .small-box {
          border-radius: 10px;
        }
      "))
    ),
    
    tabItems(
      # Onglet Vue d'ensemble
      tabItem(tabName = "equipe",
              fluidRow(
                box(
                  title = "Filtres", status = "primary", solidHeader = TRUE, width = 12,
                  radioButtons(
                    inputId = "choix_ligue", 
                    label = "Choisir la ligue :", 
                    choices = c("Toutes" = "Toutes",
                                "WSL" = "WSL", 
                                "Women's champions league" = "Women's champions league",
                                "Women's League Cup" = "Women's League Cup",
                                "FA CUP (women)" = "FA CUP (women)"), 
                    selected = "Toutes",
                    inline = TRUE
                  )
                )
              ),
              
              fluidRow(
                valueBoxOutput("box_matchs"),
                valueBoxOutput("box_buts"),
                valueBoxOutput("box_victoires")
              ),
              
              fluidRow(
                valueBoxOutput("box_defaites"),
                valueBoxOutput("box_egalites"),
                valueBoxOutput("box_but_encaisses")
              ),
              
              fluidRow(
                box(
                  title = "Performance par ligue", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("graphique_performance")
                )
              )
      ),
      
      # Onglet Statistiques
      tabItem(tabName = "statistiques",
              fluidRow(
                box(
                  title = "Évolution des résultats", status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("evolution_resultats")
                )
              ),
              
              fluidRow(
                box(
                  title = "Répartition des lieux de match", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("repartition_lieux")
                ),
                box(
                  title = "Buteuses principales", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("top_buteuses")
                )
              )
      ),
      
      # Onglet Joueuses
      tabItem(tabName = "joueuses",
              fluidRow(
                box(
                  title = "Filtres", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("filtre_poste", "Poste :",
                                       choices = c("Tous" = "Tous"),
                                       selected = "Tous")
                    ),
                    column(4,
                           selectInput("filtre_ligue_joueuses", "Ligue :",
                                       choices = c("Général" = "general",
                                                   "WSL" = "WSL",
                                                   "Women's champions league" = "Women's champions league",
                                                   "Women's League Cup" = "Women's League Cup",
                                                   "FA CUP (women)" = "FA CUP (women)"),
                                       selected = "general")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Statistiques des joueuses", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("tableau_joueuses")
                )
              ),
              
              fluidRow(
                box(
                  title = "Top scoreuses", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("top_scoreuses")
                ),
                box(
                  title = "Minutes jouées", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("minutes_jouees")
                )
              )
      ),
      
      # Onglet Matchs
      tabItem(tabName = "matchs",
              fluidRow(
                box(
                  title = "Liste des matchs", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("tableau_matchs")
                )
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Mise à jour des choix de poste
  observe({
    postes <- unique(arsenal_joueuses$Poste)
    updateSelectInput(session, "filtre_poste",
                      choices = c("Tous" = "Tous", setNames(postes, postes)))
  })
  
  # Données filtrées pour l'équipe
  donnees_filtrees <- reactive({
    if(input$choix_ligue == "Toutes") {
      arsenal24_25
    } else {
      arsenal24_25 %>%
        filter(Ligue == input$choix_ligue)
    }
  })
  
  # Value boxes
  output$box_matchs <- renderValueBox({
    valueBox(
      value = nrow(donnees_filtrees()),
      subtitle = "Matchs joués",
      icon = icon("futbol"),
      color = "blue"
    )
  })
  
  output$box_buts <- renderValueBox({
    total_buts <- sum(donnees_filtrees()$`Buts_marqués`, na.rm = TRUE)
    valueBox(
      value = total_buts,
      subtitle = "Buts marqués",
      icon = icon("bullseye"),
      color = "green"
    )
  })
  
  output$box_victoires <- renderValueBox({
    total_victoires <- donnees_filtrees() %>%
      filter(Résultat == "Victoire") %>%
      nrow()
    valueBox(
      value = total_victoires,
      subtitle = "Victoires",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  output$box_defaites <- renderValueBox({
    total_defaites <- donnees_filtrees() %>%
      filter(Résultat == "Défaite") %>%
      nrow()
    valueBox(
      value = total_defaites,
      subtitle = "Défaites",
      icon = icon("times"),
      color = "red"
    )
  })
  
  output$box_egalites <- renderValueBox({
    total_egalites <- donnees_filtrees() %>%
      filter(Résultat == "Équalité") %>%
      nrow()
    valueBox(
      value = total_egalites,
      subtitle = "Égalités",
      icon = icon("handshake"),
      color = "orange"
    )
  })
  
  output$box_but_encaisses <- renderValueBox({
    total_encaisses <- sum(donnees_filtrees()$`Buts_encaissés`, na.rm = TRUE)
    valueBox(
      value = total_encaisses,
      subtitle = "Buts encaissés",
      icon = icon("shield-alt"),
      color = "purple"
    )
  })
  
  # Graphique performance par ligue
  output$graphique_performance <- renderPlotly({
    perf_data <- arsenal24_25 %>%
      group_by(Ligue, Résultat) %>%
      summarise(n = n(), .groups = "drop")
    
    p <- ggplot(perf_data, aes(x = Ligue, y = n, fill = Résultat)) +
      geom_col(position = "dodge") +
      labs(title = "Résultats par ligue", 
           x = "Ligue", 
           y = "Nombre de matchs",
           fill = "Résultat") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Évolution des résultats
  output$evolution_resultats <- renderPlotly({
    evolution_data <- arsenal24_25 %>%
      mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
      arrange(Date) %>%
      filter(Ligue == "WSL") %>%
      mutate(
        Points = case_when(
          Résultat == "Victoire" ~ 3,
          Résultat == "Équalité" ~ 1,
          TRUE ~ 0
        ),
        Points = cumsum(Points)
      )
    
    p <- ggplot(evolution_data, aes(x = Date, y = Points)) +
      geom_line(color = "black", size = 1) +
      geom_point(aes(color = Résultat), size = 3) +
      labs(title = "Évolution des points cumulés", 
           x = "Date", 
           y = "Points :") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Répartition des lieux
  output$repartition_lieux <- renderPlotly({
    lieux_data <- arsenal24_25 %>%
      count(Lieux)
    
    plot_ly(lieux_data, labels = ~Lieux, values = ~n, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent') %>%
      layout(title = "Répartition Domicile/Extérieur")
  })
  
  # Top buteuses (à partir des noms dans la colonne Buteuses)
  output$top_buteuses <- renderPlotly({
    # Extraire les buteuses de chaque match
    buteuses_list <- arsenal24_25$Buteuses[!is.na(arsenal24_25$Buteuses)]
    buteuses_split <- unlist(strsplit(buteuses_list, ";"))
    buteuses_clean <- trimws(buteuses_split)
    
    if(length(buteuses_clean) > 0) {
      buteuses_count <- table(buteuses_clean)
      buteuses_df <- data.frame(
        Joueuse = names(buteuses_count),
        Buts = as.numeric(buteuses_count)
      ) %>%
        arrange(desc(Buts)) %>%
        head(10)
      
      p <- ggplot(buteuses_df, aes(x = reorder(Joueuse, Buts), y = Buts)) +
        geom_col(fill = "red") +
        coord_flip() +
        labs(title = "Top 10 buteuses", x = "Joueuse", y = "Nombre de buts") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Données joueuses filtrées
  joueuses_filtrees <- reactive({
    data <- arsenal_joueuses
    
    if(input$filtre_poste != "Tous") {
      data <- data %>% filter(Poste == input$filtre_poste)
    }
    
    data
  })
  
  # Tableau des joueuses
  output$tableau_joueuses <- DT::renderDataTable({
    if(input$filtre_ligue_joueuses == "general") {
      joueuses_filtrees() %>%
        select(Prénom, Nom, `Nationalité`, `Numéro`, Poste, 
               `Matchs joué`, `Minutes jouées`, `But marqués`)
    } else {
      # Colonnes spécifiques à la ligue
      ligue <- input$filtre_ligue_joueuses
      cols_base <- c("Prénom", "Nom", "Nationalité", "Numéro", "Poste")
      cols_ligue <- paste(ligue, c("matchs joués", "Minutes jouées", "Buts marqués", "Assistances"), sep = " - ")
      
      # Vérifier quelles colonnes existent
      cols_existantes <- intersect(names(joueuses_filtrees()), c(cols_base, cols_ligue))
      
      joueuses_filtrees() %>%
        select(all_of(cols_existantes))
    }
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # Top scoreurs
  output$top_scoreuses <- renderPlotly({
    data <- joueuses_filtrees() %>%
      filter(`But marqués` > 0) %>%
      arrange(desc(`But marqués`)) %>%
      head(10)
    
    if(nrow(data) > 0) {
      p <- ggplot(data, aes(x = reorder(paste(Prénom, Nom), `But marqués`), 
                            y = `But marqués`)) +
        geom_col(fill = "darkgreen") +
        coord_flip() +
        labs(title = "Top scoreurs", x = "Joueuse", y = "Buts marqués") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Minutes jouées
  output$minutes_jouees <- renderPlotly({
    data <- joueuses_filtrees() %>%
      arrange(desc(`Minutes jouées`)) %>%
      head(10)
    
    p <- ggplot(data, aes(x = reorder(paste(Prénom, Nom), `Minutes jouées`), 
                          y = `Minutes jouées`)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 - Minutes jouées", x = "Joueuse", y = "Minutes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Tableau des matchs
  output$tableau_matchs <- DT::renderDataTable({
    arsenal24_25 %>%
      mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
      arrange(desc(Date)) %>%
      select(Date, Ligue, `Équipe adverse`, Lieux, Score, Résultat, 
             `Buts_marqués`, `Buts_encaissés`, Buteuses)
  }, options = list(pageLength = 15, scrollX = TRUE))
}

# Lancer l'application
shinyApp(ui = ui, server = server)
