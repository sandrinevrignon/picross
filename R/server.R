

server <- function(input, output, session) {

  # Initialisation ----


  ## Création des réactifs ----

  myReactives <- reactiveValues()

  alerte <- reactiveVal()
  alerte(TRUE)

  grille <- reactiveVal()
  boutons <- reactiveVal()

  # solution cachée au début

  shinyjs::disable("submit")
  shinyjs::hide("ind_hor_box")
  shinyjs::hide("verif_box")
  shinyjs::hide("matrice_box")
  shinyjs::hide("ind_vert_box")



  ## Message début ----

  observe({

    if (alerte() == TRUE) {

      shinyalert("Jouer à Shiny Picross !", "<p style='font-size: 100%'>

      Bienvenue dans le merveilleux monde de la relaxation et du casse-tête !
             <br><br>

      Ce mini-jeu dans lequel vous pouvez choisir plusieurs niveaux de difficulté et tailles de grille
      vous assurera des heures et des heures de bonheur et d'épanouissement.
             <br><br>

      Sponsorisé par Mario.

      <br><br>
      Sandrine & XXXX.
             </p>
             ", type = "success", size="m", html=T, confirmButtonText="C'est parti !", confirmButtonCol='#B74d53') #B74d53 #f3969a #074c87


    }

    alerte(FALSE)

  })

  ## Texte description ----
  output$description_accueil <-renderUI(HTML("<b>Les règles du Picross :</b><br/><br/>
                                             Le but d'un Picross est de colorer les cases de la grille afin de faire apparaître une image.  <br/>
                                             Les nombres à gauche et au dessus de la grille sont là pour vous aider à déduire les cases à colorer. <br/><br/>
                                             Sélectionnez d'abord le niveau de difficulté et la taille de la grille souhaités avant de rejoindre la page \"Jouer\".<br/>
                                             Si Vous souhaitez lancer une nouvelle partie, choisissez \"Quitter\" dans l'onglet Jouer, avant de revenir ici choisir les nouveaux paramètres.<br/>
                                             Il est possible d'obtenir un lien de sauvegarde via l'onglet \"Sauvegarder\". "))

  ## Texte choisir paramètres ----

  output$texte_choisir <-renderUI(HTML("<b>Choisissez une taille de grille dans \"Sélection des options\".</b>"))

  ## Initiatilisation texte verif ----
  output$texte_verif <-renderUI(HTML(" "))

  ## Select taille ----
  output$select_taille <- renderUI({ # Choisissez une taille de grille
    radioButtons('select_taille', '', choiceNames=c("5x5", "10x10", "15x15", "20x20"), choiceValues=c("5x5", "10x10", "15x15", "20x20"), selected = character(0))
  })

  # Choix de la grille en fonction des options ----
  observeEvent(input$submit , {
    name_grille=paste(input$select_taille, sample(c(1:2), 1), sep="_")

    grille(as.data.frame(readxl::read_xlsx(paste0(dirname(getwd()),"/data/grilles.xlsx"), sheet = name_grille, col_names = FALSE, .name_repair = "unique_quiet")))

    ind_vert <- as.list(apply(grille(), MARGIN=2, function(x){attributes(gregexpr("1+", paste(x,collapse=""))[[1]])[["match.length"]]}))
    ind_hor <- as.list(apply(grille(), MARGIN=1, function(x){attributes(gregexpr("1+", paste(x,collapse=""))[[1]])[["match.length"]]}))

    ind_vert = stri_list2matrix(ind_vert, byrow=FALSE)
    ind_hor = stri_list2matrix(ind_hor, byrow=TRUE)

    ind_vert[which(ind_vert==-1)]=0 # Pour les cas où une colonne/ligne est vide, il y avait un pb
    ind_hor[which(ind_hor==-1)]=0

    ind_vert = apply(ind_vert, MARGIN=2, function(x){c(rep(NA, length(which(is.na(x)))), x[1:(length(x)-length(which(is.na(x))))])})

    ind_vert[is.na(ind_vert)]=" "
    ind_hor[is.na(ind_hor)]=" "

    ## Création grille ----
    output$matrice_boutons <- renderUI({
      nRows <- dim(grille())[1]    # Définir le nombre de lignes
      nCols <- dim(grille())[2]     # Définir le nombre de colonnes
      # Créer une matrice de boutons
      boutons <- lapply(1:(nRows * nCols), function(i) {
        actionButton(inputId = paste0("bouton", i), label = "")
      })
      # Organiser les boutons en grille
      tagList(div(class = "btn-grid",
                  lapply(split(boutons, ceiling(seq_along(boutons) / nCols)), div, class = "button-row")))
    })

    # Création indications verticales ----
    output$matrice_ind_vert <- renderUI({
      nRows <- dim(ind_vert)[1]    # Définir le nombre de lignes
      nCols <- dim(ind_vert)[2]    # Définir le nombre de colonnes
      # Créer une matrice de boutons
      boutons <- lapply(1:(nRows * nCols), function(i) {
        actionButton(inputId = paste0("ind_vert", i), label = HTML(paste0("<b>",t(ind_vert)[i], "</b>")), style="background-color: #fff; border-color: #2e6da4") #color: #337ab7;
      })
      # Organiser les boutons en grille
      tagList(div(class = "btn-grid",
                  lapply(split(boutons, ceiling(seq_along(boutons) / nCols)), div, class = "button-row")))
    })

    # Création indications horizontales ----
    output$matrice_ind_hor <- renderUI({
      nRows <- dim(ind_hor)[1]    # Définir le nombre de lignes
      nCols <- dim(ind_hor)[2]    # Définir le nombre de colonnes
      # Créer une matrice de boutons
      boutons <- lapply(1:(nRows * nCols), function(i) {
        actionButton(inputId = paste0("ind_hor", i), label = HTML(paste0("<b>",t(ind_hor)[i], "</b>")), style="background-color: #fff; border-color: #2e6da4") #color: #337ab7;
      })
      # Organiser les boutons en grille
      tagList(div(class = "btn-grid",
                  lapply(split(boutons, ceiling(seq_along(boutons) / nCols)), div, class = "button-row")))
    })

    # Boutons(), stock état de la grille ----
    boutons(rep(0, dim(grille())[1]*dim(grille())[2]))

    # Afficher les éléments de "Jouer" après la sélection----
    shinyjs::show("ind_hor_box")
    shinyjs::show("verif_box")
    shinyjs::show("matrice_box")
    shinyjs::show("ind_vert_box")
    shinyjs::hide("texte_choisir_box")

  })

  # Activateur boutons ----
  dimmax=100
  lapply(
    X = 1:dimmax,
    FUN = function(i){
      observeEvent(input[[paste0("bouton", i)]], {
        boutons_tempo = boutons()

        if (boutons_tempo[i] == 0){
          boutons_tempo[i]=1
          runjs(paste0('document.getElementById("bouton', i, '").style.backgroundColor = "black";'))
        }
        else if (boutons_tempo[i] == 1){
          boutons_tempo[i]=2
          runjs(paste0('document.getElementById("bouton', i, '").style.backgroundColor = "#d0d1d2";'))
        }
        else{
          boutons_tempo[i]=0
          runjs(paste0('document.getElementById("bouton', i, '").style.backgroundColor = "#f3969a";'))
        }

        boutons(boutons_tempo)

      })
    }
  )


  #Vérifier ----

  observeEvent(input$verifier , {
    boutons_tempo = boutons()
    boutons_tempo[which(boutons_tempo == 2)] = 0
    if(all(t(grille()) == boutons_tempo )){
      output$texte_verif <-renderUI(HTML("<b>Résultat :</b><br/><br/>
                                             Vous avez résolu le picross ! :) "))

    }else {
      output$texte_verif <-renderUI(HTML("<b>Résultat :</b><br/><br/>
                                             Il y a des erreurs dans le picross ! :o "))

    }
  })

  ## Activation submit --------------------

  observe({
    if (is.null(input$select_taille)){
      shinyjs::disable("submit")
    } else{
      shinyjs::enable("submit")
    }

  })


}
