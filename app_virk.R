library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

#rm(list=ls())

# to do
# ændre til ugeligt i stedet for dagligt
# kun hvis næste liste hvis buttom trykkes
# ændre navne

# tester om git virker

if (!exists('medlemmer')) {
  medlemmer <- c('Alexander',
                 'Erik',
                 'Jenny',
                 'Rasmus',
                 'Hege',
                 'Rasmus',
                 'Kristian'
  )
}

morgen_orig <- data.frame(Person = medlemmer)
morgen_orig$Person <- as.character(morgen_orig$Person)

if (!exists('morgen')) {
  morgen <- data.frame(Person = medlemmer)
  morgen$Person <- as.character(morgen$Person)
  startdato <- as.Date("2018-07-19")
  #  morgen$Dato <- seq(startdato, startdato+((nrow(morgen)-1)*7), by = 'week')
  morgen$Dato <- seq(startdato, startdato+(nrow(morgen)-1), by = 'day')
}

if(!exists('morgen2')) {
  morgen2 <- data.frame(Person = medlemmer)
  morgen2$Person <- as.character(morgen2$Person)
  startdato <- morgen$Dato[nrow(morgen)] + 1 # + 6
  #  morgen2$Dato <<- seq(startdato, startdato+((nrow(morgen2)-1)*7), by = 'week')
  morgen2$Dato <- seq(startdato, startdato+(nrow(morgen2)-1), by = 'day')
}

akt_dato <- Sys.Date()

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(
                      title = 'DATAANALYSE'
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('Morgenmadsliste', tabName = 'morgen', icon = icon('coffe')) # birthday-cake
                      )
                      
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'morgen',
                                h2('Morgenmad'),
                                hr(),
                                fluidRow(
                                  column(7,
                                         box(title = 'Morgenmadsliste', solidHeader = T, status = 'primary',
                                             DT::dataTableOutput('morgen'),
                                             width = 400
                                         ),
                                         actionButton('skip', 'Skip', icon = icon('forward')),
                                         actionButton('byt', 'Byt', icon = icon('handshake-o')),
                                         actionButton('til_afm', 'Tilmeld/afmeld', icon = icon('exchange')),
                                         actionButton('dato', 'dato+1')
                                  ),
                                  column(5,
                                         box(title = 'Næste liste', solidHeader = T, status = 'primary',
                                             DT::dataTableOutput('morgen2'),
                                             width = 400
                                         )
                                  )
                                )
                        )
                      )
                    )
)

server <- shinyServer(function(input, output, session) {
  
  # testdato
  observeEvent(input$dato, {
    akt_dato <<- as.Date(akt_dato) + 1
  })
  
  ### TILMELD/AFMELD -----------------------------------------------
  # create tilføj_afmeld_model
  til_afm_model <- function(failed = FALSE) {
    modalDialog(easyClose = T,
                fade = T,
                title = 'Tilmed eller afmeld',
                
                selectInput(inputId = 'til_afm_per',
                            label = 'Vælg handling',
                            choices = list('Tilmeld',
                                           'Afmeld')
                ),
                
                conditionalPanel(
                  condition = "input.til_afm_per == 'Afmeld'",
                  selectInput(inputId = "afmeld_pers", 
                              label = 'Vælg person',
                              choices = medlemmer),
                  checkboxInput(inputId = "afmeld_nu", 
                                label = "Afmeld også fra nuværende liste", 
                                value = FALSE),
                  checkboxInput(inputId = "afmeld_midl", 
                                label = "Afmeld kun fra nuværende liste", 
                                value = FALSE)
                ),
                
                conditionalPanel(
                  condition = "input.til_afm_per == 'Tilmeld'",
                  textInput(inputId = "Tilmeld_pers", 
                            label = 'Tilmeld person',
                            value = ''),
                  checkboxInput(inputId = "tilmeld_nu", 
                                label = "Tilmeld nuværende liste", 
                                value = FALSE)
                ),
                
                if (failed) { 
                  if (input$til_afm_per == 'Tilmeld') {
                    div(tags$b(paste("Hov!", input$Tilmeld_pers, "findes jo allerede!"), style = "color: blue;"))
                  } 
                  else if (input$til_afm_per == 'Afmeld' & input$afmeld_midl == F & input$afmeld_nu == T) {
                    div(tags$b(paste("Hov!", input$afmeld_pers, "har enten haft kage med i denne omgang, skal 
                                     have kage med næste gang, eller findes ikke på den nuværende liste.", 
                                     input$afmeld_pers, "er afmeldt fra næste 'runde'."),
                               style = "color: blue;"))
                  }
                  else {
                    div(tags$b(paste("Hov! Der skete en fejl"),
                               style = "color: blue;"))
                  }
                },
                
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("ok_til_afm", "OK")
                )
                
                    )
                }
  
  # Show til_afm_model when button is clicked.
  observeEvent(input$til_afm, {
    showModal(til_afm_model())
  })
  
  # oberve tilmeld/afmeld event
  observeEvent(input$ok_til_afm, { 
    
    if (input$til_afm_per == 'Afmeld') {
      
      if(input$afmeld_nu == T & input$afmeld_midl == T) {
        showModal(til_afm_model(failed = TRUE))
      }
      
      else if (input$afmeld_midl == F) {
        
        morgen_orig <<- subset(morgen_orig, Person != input$afmeld_pers)
        
        idx_afmeld <<- which(medlemmer %in% input$afmeld_pers)
        medlemmer <<- medlemmer[-idx_afmeld]
        
        afm_pers_m2_index <<- which(morgen2$Person == input$afmeld_pers)
        morgen2 <<- subset(morgen2, Person != input$afmeld_pers)
        morgen2$Dato <<- seq(morgen$Dato[nrow(morgen)] + 1, 
                             morgen$Dato[nrow(morgen)] + nrow(morgen2), 
                             by = 1) # + 7 
        
        if(input$afmeld_nu == T) {
          if (input$afmeld_pers %in% morgen$Person) {
            afm_akt_index <<- which(morgen$Dato == as.Date(akt_dato)) # sys.Date() kode til rettes til uge-basis
            
            if (sum(morgen$Person == input$afmeld_pers) == 1) {
              if (morgen$Person[afm_akt_index] == input$afmeld_pers) {
                showModal(til_afm_model(failed = TRUE))
              }
              else if (which(morgen$Person == input$afmeld_pers) < afm_akt_index) {
                showModal(til_afm_model(failed = TRUE))
              }
              
              else {
                afm_pers_index <- which(morgen$Person == input$afmeld_pers)
                
                morgen <<- subset(morgen, Person != input$afmeld_pers)
                
                if (afm_pers_index != nrow(morgen)+1) {
                  morgen$Dato[(afm_pers_index):nrow(morgen)] <<- morgen$Dato[(afm_pers_index):nrow(morgen)] - 1  
                }
                
                morgen2$Dato <<- morgen2$Dato - 1
                
                removeModal()
              }
            }
            else if (sum(morgen$Person == input$afmeld_pers) == 2) {
              afm_pers_index <<- which(morgen$Person == input$afmeld_pers)  
              
              if (morgen$Person[afm_akt_index] == input$afmeld_pers) {
                showModal(til_afm_model(failed = TRUE))
              }
              else if (afm_pers_index[1] < afm_akt_index & afm_pers_index[2] < afm_akt_index) {
                showModal(til_afm_model(failed = TRUE))
              } 
              else if (afm_pers_index[1] < afm_akt_index & afm_pers_index[2] > afm_akt_index) {
                
                afm_pers_index_2  <- which(morgen$Person == input$afmeld_pers)[2]
                morgen <<- morgen[-afm_pers_index_2,]
                
                if (afm_pers_index_2 != nrow(morgen)+1) {
                  morgen$Dato[(afm_pers_index_2):nrow(morgen)] <<- morgen$Dato[(afm_pers_index_2):nrow(morgen)] - 1  
                }
                
                morgen2$Dato <<- morgen2$Dato - 1
                removeModal()
              }
              else if (afm_pers_index[1] > afm_akt_index & afm_pers_index[2] > afm_akt_index) {
                
                morgen <<- morgen[-afm_pers_index,]
                morgen$Dato <<- seq(morgen$Dato[1], morgen$Dato[1] + nrow(morgen) - 1, by = 'day')
                
                morgen2$Dato <<- morgen2$Dato - 2
                removeModal()
              }
            }
            else {
              showModal(til_afm_model(failed = TRUE))
            }
            
          } 
          else {
            showModal(til_afm_model(failed = TRUE))
          }
          
        } else {
          removeModal()
        }  
      } 
      else if (input$afmeld_midl == T) {
        if (input$afmeld_pers %in% morgen$Person) {
          afmeld_midl_index <- which(morgen$Dato == as.Date(akt_dato)) # sys.Date() kode til rettes til uge-basis
          
          if (morgen$Person[afmeld_midl_index] == input$afmeld_pers) {
            showModal(til_afm_model(failed = TRUE))
          }
          else if (which(morgen$Person == input$afmeld_pers) < afmeld_midl_index) {
            showModal(til_afm_model(failed = TRUE))
          }
          else {
            afmeld_person_midl_index <- which(morgen$Person == input$afmeld_pers)
            
            morgen <<- subset(morgen, Person != input$afmeld_pers)
            
            if (afmeld_person_midl_index != nrow(morgen)+1) {
              morgen$Dato[(afmeld_person_midl_index):nrow(morgen)] <<- morgen$Dato[(afmeld_person_midl_index):nrow(morgen)] - 1  
            }
            
            morgen2$Dato <<- morgen2$Dato - 1
            
            removeModal()
          }
        }
        else {
          showModal(til_afm_model(failed = TRUE))
        }
        
        
      }
    }
    
    if (input$til_afm_per == 'Tilmeld') {
      if (!(tolower(trimws(input$Tilmeld_pers)) %in% tolower(trimws(morgen_orig$Person)))) {
        
        morgen_orig <<- rbind(morgen_orig, input$Tilmeld_pers) %>% arrange(Person)
        medlemmer <<- c(medlemmer, input$Tilmeld_pers) %>% sort()
        
        tmp_m2_idx <- c(morgen2$Person, input$Tilmeld_pers) %>% sort()
        tmp_m2_idx <- which(tmp_m2_idx %in% input$Tilmeld_pers)
        
        if (tmp_m2_idx == 1) {
          morgen2 <<- rbind(data.frame(Person = input$Tilmeld_pers, Dato = morgen2$Dato[tmp_m2_idx]),
                            morgen2)
          morgen2$Dato[2:nrow(morgen2)] <<- morgen2$Dato[2:nrow(morgen2)] + 1
        }
        else if (tmp_m2_idx == nrow(morgen2) + 1) {
          morgen2 <<- rbind(morgen2,
                            data.frame(Person = input$Tilmeld_pers, Dato = morgen2$Dato[tmp_m2_idx-1] + 1))
        } 
        else {
          morgen2 <<- rbind(morgen2[1:(tmp_m2_idx-1),],
                            data.frame(Person = input$Tilmeld_pers, Dato = morgen2$Dato[tmp_m2_idx]),
                            morgen2[tmp_m2_idx:nrow(morgen2),])
          morgen2$Dato[(tmp_m2_idx+1):nrow(morgen2)] <<- morgen2$Dato[(tmp_m2_idx+1):nrow(morgen2)] + 1
        }
        
        if(input$tilmeld_nu == T) {
          if(!(input$Tilmeld_pers %in% morgen$Person)) {
            #which(morgen$Dato %in% seq(dags_dato-7, as.Date(akt_dato), by = 'day'))
            
            dato_input <- format(morgen$Dato[nrow(morgen)] + 1, format = '%Y-%m-%d') # + 6
            
            morgen <<- rbind(morgen, c(Person = input$Tilmeld_pers, Dato = dato_input)) 
            morgen2$Dato <<- morgen2$Dato + 1
            
            removeModal()
            
          } else {
            showModal(til_afm_model(failed = TRUE))
          }
        }
        if(input$tilmeld_nu == F) {
          removeModal()
        }  
      }
      else {
        showModal(til_afm_model(failed = TRUE))
      }
    }
  })
  
  ### BYT --------------------------------------------  
  # creating byt_modal  
  byt_Modal <- function(failed = FALSE) {
    modalDialog(easyClose = T,
                fade = T,
                title = 'BYT',
                
                fluidRow(
                  column(4,
                         selectInput(inputId = 'byt1',
                                     label = 'Vælg person',
                                     choices = c(medlemmer,
                                                 'Manuel')
                         )
                  ),
                  column(4,
                         dateInput(inputId = 'dato_byt1', 
                                   label = 'Vælg dato'
                         )
                  )
                ),
                
                fluidRow(
                  column(4,
                         selectInput(inputId = 'byt2',
                                     label = 'Byt med',
                                     choices = c(medlemmer,
                                                 'Manuel')
                         )
                  ),
                  column(4, 
                         dateInput(inputId = 'dato_byt2', 
                                   label = 'Vælg dato'
                         )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.byt1 == 'Manuel' & 
                  input.byt2 == 'Manuel'",
                  selectInput(inputId = "manuel_byt_ud", 
                              label = 'Vælg person',
                              choices = morgen$Person),
                  dateInput(inputId = 'manuel_dato_ud', 
                            label = 'Vælg dato'),
                  selectInput(inputId = "manuel_byt_ind", 
                              label = 'Vælg ny person',
                              choices = medlemmer),
                  dateInput(inputId = 'manuel_dato_ind', 
                            label = 'Vælg ny dato')
                ),
                
                if (failed) { # conditional panel
                  div(tags$b("Dette bytte er ikke muligt", style = "color: blue;"))
                },
                
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("ok_byt", "OK")
                )
                
)
  }
  
  # Show byt_Modal when button is clicked.
  observeEvent(input$byt, {
    showModal(byt_Modal())
  })
  
  # Observe byt event
  observeEvent(input$ok_byt, {
    
    if(input$byt1 == 'Manuel' & input$byt2 == 'Manuel') {
      
      if(input$manuel_dato_ud %in% morgen$Dato) {
        
        morgen$Person[morgen$Person == input$manuel_byt_ud & 
                        morgen$Dato == input$manuel_dato_ud] <<- input$manuel_byt_ind
        
        morgen$Dato[morgen$Person == input$manuel_byt_ud &
                      morgen$Dato == input$manuel_dato_ud] <<- input$manuel_dato_ind 
        
        removeModal()
      }
      else {
        showModal(byt_Modal(failed = TRUE))
      }
    }
    else if (input$byt1 == 'Manuel' | input$byt2 == 'Manuel'){
      showModal(byt_Modal(failed = TRUE))
    }
    else {
      
      morgen_1og2_tmp <<- rbind(morgen, morgen2)
      
      byt1_index <<- which(morgen_1og2_tmp$Person %in% input$byt1 & 
                             morgen_1og2_tmp$Dato %in% input$dato_byt1) 
      
      byt2_index <<- which(morgen_1og2_tmp$Person %in% input$byt2 & 
                             morgen_1og2_tmp$Dato %in% input$dato_byt2) 
      
      if(length(byt1_index) == 1 & length(byt2_index) == 1) {
        morgen_1og2_tmp$Person[byt1_index] <<- input$byt2
        morgen_1og2_tmp$Person[byt2_index] <<- input$byt1
        
        morgen <<- morgen_1og2_tmp[1:nrow(morgen),]
        morgen2 <<- morgen_1og2_tmp[(nrow(morgen)+1):nrow(morgen_1og2_tmp),]
        
        removeModal() 
      }
      else {
        showModal(byt_Modal(failed = TRUE))
      }      
      
    }
  })
  
  ### SKIP --------------------------------------
  
  skip_Modal <- function(failed = FALSE) {
    modalDialog(easyClose = T,
                fade = T,
                title = 'SKIP',
                
                selectInput(inputId = 'skip_slet',
                            label = 'Vælg handling',
                            choices = list('Skip',
                                           'Slet',
                                           'Tilføj')),
                
                dateInput(inputId = 'dato_skip', 
                          label = 'Vælg dato'),
                
                conditionalPanel(
                  condition = "input.skip_slet == 'Tilføj'",
                  selectInput(inputId = "tilføj_pers", 
                              label = 'Tilføj',
                              choices =  medlemmer)
                ),
                
                if (failed) { 
                  if(input$skip_slet == 'Tilføj') {
                    div(tags$b("Hov, der findes allerede en på denne dato", style = "color: blue;"))
                  }
                  else {
                    div(tags$b("Hov, er der valgt en korrekt dato?", style = "color: blue;"))
                  }
                  
                },
                
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("ok_skip", "OK")
                )
                
    )}
  
  # Show til_afm_model when button is clicked.
  observeEvent(input$skip, {
    showModal(skip_Modal())
  })
  
  # when the skip button is clicked, skip to next week  
  observeEvent(input$ok_skip, {
    
    if (input$skip_slet == 'Skip') {   
      
      if (input$dato_skip %in% morgen$Dato) {
        
        skip_index <- which(morgen$Dato == input$dato_skip)
        
        if(skip_index == 1) {
          
          morgen <<- rbind(data.frame(Person = 'Ingen morgenmad', 
                                      Dato = morgen$Dato[skip_index]), 
                           morgen[skip_index:nrow(morgen),])
          
        }
        
        if(skip_index > 1) { 
          
          morgen <<- rbind(morgen[1:(skip_index-1),], 
                           data.frame(Person = 'Ingen morgenmad', 
                                      Dato = morgen$Dato[skip_index]), 
                           morgen[skip_index:nrow(morgen),])
        }
        
        morgen$Dato[(skip_index+1):nrow(morgen)] <<- morgen$Dato[(skip_index+1):nrow(morgen)] + 1
        
        morgen2$Dato <<- morgen2$Dato + 1
        
        removeModal()
      }
      else if (input$dato_skip %in% morgen2$Dato) {
        
        skip_index2 <- which(morgen2$Dato == input$dato_skip)
        
        if (skip_index2 == 1) {
          
          morgen2 <<- rbind(data.frame(Person = 'Ingen morgenmad', 
                                       Dato = morgen2$Dato[skip_index2]), 
                            morgen2[skip_index2:nrow(morgen2),])
          
        }
        
        if(skip_index2 > 1) { 
          
          morgen2 <<- rbind(morgen2[1:(skip_index2-1),], 
                            data.frame(Person = 'Ingen morgenmad', 
                                       Dato = morgen2$Dato[skip_index2]), 
                            morgen2[skip_index2:nrow(morgen2),])
        }
        
        morgen2$Dato[(skip_index2+1):nrow(morgen2)] <<- morgen2$Dato[(skip_index2+1):nrow(morgen2)] + 1
        
        removeModal()
      }
      else{
        showModal(skip_Modal(failed = TRUE))
      }
    }
    
    if (input$skip_slet == 'Slet') {
      
      if (input$dato_skip %in% morgen$Dato) {
        
        skip_slet <- which(morgen$Dato == input$dato_skip)
        
        if (which(morgen$Dato == as.Date(akt_dato)) %in% skip_slet) { # %in% seq(Sys.Date()-7, Sys.Date(), by = 'day')
          showModal(skip_Modal(failed = TRUE))
        }
        else {
          
          if (which(morgen$Dato == input$dato_skip) > which(morgen$Dato == as.Date(akt_dato))) {
            morgen2$Dato <<- morgen2$Dato - 1  
          }
          
          morgen <<- subset(morgen, Dato != input$dato_skip)
          
          if (min(skip_slet) != nrow(morgen) + 1 & min(skip_slet) != 1) {
            morgen$Dato[(min(skip_slet)):nrow(morgen)] <<- morgen$Dato[(min(skip_slet)):nrow(morgen)] - 1  
          }
          
          removeModal()
        }
      }
      else {
        showModal(skip_Modal(failed = TRUE))
      }
    }
    
    if (input$skip_slet == 'Tilføj') {
      
      if (input$dato_skip %in% morgen$Dato) {
        showModal(skip_Modal(failed = TRUE))
      }
      else {
        dato_input <- format(input$dato_skip, format = '%Y-%m-%d')
        
        morgen <<- rbind(morgen, c(Person = input$tilføj_pers, Dato = dato_input))
        
        morgen <<- arrange(morgen, Dato) 
        
        startdato <- morgen$Dato[nrow(morgen)] + 1
        morgen2$Dato <<- seq(startdato, startdato + (nrow(morgen2)-1), by = 'day')
        
        removeModal()
      }
    }  
    
  })
  
  ### OUTPUT TABEL ----------------------------------
  output$morgen <- DT::renderDataTable({
    input$ok_skip
    input$ok_byt
    input$ok_til_afm
    
    #index <- which(morgen$Dato %in% seq(dags_dato-7, as.Date(akt_dato), by = 'day'))
    index <<- which(morgen$Dato == as.Date(akt_dato))
    
    if(length(index) == 0) {
      
      # gemmer den gamle liste
      morgen_gl <<- morgen
      
      # morgen2 bliver nu morgen
      morgen <<- morgen2 
      
      #index <- which(morgen$Dato %in% seq(dags_dato-7, as.Date(akt_dato), by = 'day'))
      index <- which(morgen$Dato == as.Date(akt_dato))
      
      # konstruerer næste liste, morgen2
      startdato <- morgen$Dato[nrow(morgen)] + 1 # + 6
      morgen2 <<- morgen_orig 
      #morgen$Dato <<- seq(startdato, startdato+((nrow(morgen)-1)*7), by = 'week')
      morgen2$Dato <<- seq(startdato, startdato+(nrow(morgen2)-1), by = 'day')
      
    }
    
    morgen$Dato <- format(morgen$Dato, format = '%A %d. %B')
    morgen2$Dato <- format(morgen2$Dato, format = '%A %d. %B')
    
    morgen_output <<- rbind(morgen, morgen2)
    
    DT::datatable(morgen_output[(index-1):(index+5),], 
                  options = list(paging = F, 
                                 searching = F, 
                                 bInfo = F), 
                  rownames= FALSE) %>% formatStyle(
                    'Dato',
                    target = 'row',
                    backgroundColor = styleEqual(morgen_output$Dato[index], c('lightblue'))
                  )
    #https://rstudio.github.io/DT/010-style.html
  })
  
  
  ### OUTPUT TABEL2 ----------------------------------
  output$morgen2 <- DT::renderDataTable({
    input$ok_skip
    input$ok_til_afm
    input$ok_byt
    
    morgen2$Dato <- format(morgen2$Dato, format = '%A %d. %B')
    
    DT::datatable(morgen2, 
                  options = list(paging = F, 
                                 searching = F, 
                                 bInfo = F), 
                  rownames = FALSE)
  })
  
})


shinyApp(ui=ui, server = server)

