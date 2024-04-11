library(shiny)
library(shinyjs)
library(bslib)
library(shinyalert)
library(stringi)

#' Picross game
#' @name Picross
#'
#' @param request
#'
#' @return grille de picross avec solution
#' @export
#'
#' @examples grille de picross 5x5 en forme de S
function(request){navbarPage(

    tags$link(
    rel = "stylesheet",
    href="https://fonts.googleapis.com/css?family=Tangerine"
  ),

  tags$style(
    "
  * {
font-family: Lucida Sans;
font-size: 15px;
  }

    .custom_description {
    font-size: 17px;
    font-family: Lucida Sans;
      border-radius: 15px;
border-style: solid;
border-color: #560000;

 padding: 15px

    }

  .btn {
    height: 20px;
    width: 20px;
    border: 10px;
    margin-top: 2px;
    margin-bottom: 2px;
    margin-left: 2px;
    margin-right: 2px;
  }

  }
  "
  ),


  theme = bs_theme(version = 4, bootswatch = "minty"),

  useShinyjs(),

  ## PAGE ACCUEIL ET OPTIONS ----
  tabPanel(

    #Name
    "Sélection des options",

    fluidRow(
      style="opacity:1; background-color: white ;margin-top: 0px; width: 100%;",
      column(5,offset=0, align="justify",

             fluidRow(
               br(),
               ### Image Mario ----
               img(src="image.png",height='320px',width='380px'),

               hr(),

               br(),
               ### Sélection taille et difficulté ----
               shinydashboard::box(id = "selection_box", width=12,

                                   tags$b("Choisissez une taille de grille"),br(),
                                   uiOutput("select_taille"),
               ),
               actionButton('submit', label = "Sélectionner puis cliquer sur Jouer (en haut)", style= "width: 35%; height: 100%"),
             )
      ),

      column(6,offset=0, align="justify",
             br(),

             ### Description ----
             htmlOutput("description_accueil", class="custom_description"),
             br(),
             br(),

      )),

    br(),br(),
  ),

  ## PAGE JOUER ----
  tabPanel(

    #Name
    "Jouer",

    fluidRow(
      column(6,offset=0, align="justify",

             ### Message choisir paramètres ----

             shinydashboard::box(id = "texte_choisir_box", width=12,
                                 htmlOutput("texte_choisir", class="custom_description"),
             ),

             ### Indications verticales ----
             shinydashboard::box(id = "ind_vert_box", width=12,
                                 uiOutput("matrice_ind_vert"),
             ),
             br(),
      )
    ),
    fluidRow(

      ### Picross ----
      column(8,offset=0, align="justify",
             shinydashboard::box(id = "matrice_box", width=12,
                                 uiOutput("matrice_boutons"),
             ),
             br(),

             ### Vérifier ----
             shinydashboard::box(id = "verif_box", width=6,
                                 br(), br(),
                                 actionButton('verifier', label = "Vérifier", style= "width: 35%; height: 30%"),
                                 br(),

                                 htmlOutput("texte_verif", class="custom_description"),

             ),
      ),

      ### Indications horizontales ----

      column(3,offset=0, align="justify",
             shinydashboard::box(id = "ind_hor_box", width=12,
                                 uiOutput("matrice_ind_hor"),
             ),
             br(), br(), br(),

             # shinydashboard::box(id = "description_quete", width=4, status="primary", collapsible=T, htmlOutput("description_quete", class="custom_description_quete")),
      )
    )

  ),


  #Close the shinyUI
)}
