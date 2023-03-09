library(shiny)
library(ggplot2)
library(ggtext)
library(waffle)
library(data.table)
library(dplyr)
library(shinydashboard)

factions = c("The Arborec",
             "The Barony of Letnev",
             "The Clan of Saar",
             "The Embers of Muaat",
             "The Emirates of Hacan",
             "The Federation of Sol",
             "The Ghosts of Creuss",
             "The L1Z1X Mindnet",
             "The Mentak Coalition",
             "The Naalu Collective",
             "The Nekro Virus",
             "The Sardakk N’orr",
             "The Universities of Jol-Nar",
             "The Winnu",
             "The Xxcha Kingdom",
             "The Yin Brotherhood",
             "The Yssaril Tribes")

ui <- function(requests) {
    fluidPage(
        fluidRow(
            box(width = 3,
                title = ("Options"),
                collapsible = T,
                checkboxInput("include_expansion", "Include Prophecy of Kings expansion"),
                checkboxInput("randomize_seats", "Randomize seats", value = T),
                tags$div(title = "A specific seed number will always generate the same results. If left blank, a random number will be generated.",
                textInput("seed", "Set seed")),
                
                tags$div(title = "How many people are playing?",
                    numericInput("nplayers", "Number of players", min = 2, max = 6, step = 1, value = 6)
                    ),
                tags$div(title = "How many factions should each player get to choose between?",
                    numericInput("nfactionspp", "Number of factions per player", min = 1, max = 2, step = 1, value = 2)
                    ),
                checkboxGroupInput("allfactions", "Include factions", choices = factions, inline = F, selected = factions)
                
            ),

            box(width = 9,
                title = ("Player settings"),
                collapsible = T,
                tags$div(title = "Assign player names. These fields can be left blank.",
                    wellPanel(
                        uiOutput("ui1")
                        )
                    ),
                tags$div(title="Assign easier to play with factions for any player.",
                    selectInput("handicap", "Prioritize 'beginner' factions for", choices = list(), multiple = T),
                    checkboxInput("speaker", "Assign speaker")
                    ),
                actionButton("assign", "Assign", icon = icon("random")),
                bookmarkButton(id = "bookmark1", label = "Share", icon = icon("share"))
            ),
                
            box(width = 9,
                plotOutput("results")
                
            )
        )
    )
}

server <- function(input, output, session) {
    
    setBookmarkExclude("bookmark1")
    
    observeEvent(input$bookmark1, {
        session$doBookmark()
    })
    
    onBookmark(function(state) {
        state$values$seed <- fdata$seed
        state$values$parts <- fdata$parts
        
    })
    onRestore(function(state) {
        fdata$seed <- state$values$seed
        updateTextInput(session, "seed", "Set seed", placeholder = as.character(fdata$seed))

        output$results <- renderPlot({
            waffle(unlist(state$values$parts), rows = 2, legend_pos = "bottom") + theme(legend.text = element_markdown(size = 14))
            
        })
        
    })

    
    fdata <- reactiveValues()
    
    factions = c("The Arborec",
                  "The Barony of Letnev",
                  "The Clan of Saar",
                  "The Embers of Muaat",
                  "The Emirates of Hacan",
                  "The Federation of Sol",
                  "The Ghosts of Creuss",
                  "The L1Z1X Mindnet",
                  "The Mentak Coalition",
                  "The Naalu Collective",
                  "The Nekro Virus",
                  "The Sardakk N’orr",
                  "The Universities of Jol-Nar",
                  "The Winnu",
                  "The Xxcha Kingdom",
                  "The Yin Brotherhood",
                  "The Yssaril Tribes")
    
    expansion_factions = c("The Aargent Flight",
                           "The Mahact Gene-Sorcerers",
                           "The Vuil'raith Cabal",
                           "The Nomad",
                           "The Empyrean",
                           "The Titans of Ul",
                           "The Naaz-Rokha Alliance")
    
    ranked_factions = c(7, 15, 8, 4, 5, 1, 11, 12, 14, 6, 13, 17, 2, 9, 16, 3, 10, rep(99, 7))
    ranked_factions <- data.frame(factions = c(factions, expansion_factions), ranked_factions = ranked_factions)
    selected_factions <- factions
    
    # Assign reactive values
    fdata$factions <- factions
    fdata$ranked_factions <- ranked_factions
    fdata$expansion_factions <- expansion_factions
    fdata$selected_factions = selected_factions
    
    #fdata$selected_factions <- factions
    
    output[["ui1"]] <- renderUI({
        if(is.numeric(input$nplayers)){
            i = 1:input$nplayers
            UI <- paste0("flowLayout(",
                         paste0("textInput(", 
                                "'field", i, "', ",
                                paste0("'Player ", i, "'"),
                                ")",
                                collapse = ", "),
                         ")")
            
            eval(parse(text = UI))
        }

    })
    
    get_player_names <- function(){
        rename <- function(i){
            x <- input[[paste0("field", i)]]
            if(!is.null(x)){
                if((nchar(x) == 0) | (x == " ")) x <- paste0("Player ", i)
                return(x)
            } else {
                return(NA)
            }

            
        }
        
        if(is.numeric(input$nplayers)) sapply(1:input$nplayers, FUN = function(i) rename(i), simplify = T)
    }
   
    output$results <- renderPlot({
        dat <- dat()
        assign("dat", dat, envir = .GlobalEnv)
        
        if(!is.na(input$nplayers) & !is.na(input$nfactionspp)) waffle(dat(), rows = 2, legend_pos = "bottom") + theme(legend.text = element_markdown(size = 14))

    })
    
    
    observeEvent(input$assign, {
        if(is.na(input$nplayers) | is.na(input$nfactionspp)) {
            showModal(
                modalDialog(size = "s",
                            title = "Too few factions",
                            "Select more factions or adjust your settings")  
            )
        }
    })
    
    dat <- eventReactive(input$assign, {
        
        if(is.null(input$seed) | trimws(input$seed) == "") {
            fdata$seed <- sample.int(10000000, 1)
            updateTextInput(session, "seed", "Set seed", placeholder = as.character(fdata$seed))
            set.seed(fdata$seed)
        }
        else {
            set.seed(as.integer(input$seed))
            }
        
        
        
        if(length(fdata$selected_factions) < input$nplayers * input$nfactionspp){
            showModal(
                modalDialog(size = "s",
                            title = "Too few factions",
                            "Select more factions or adjust your settings")    
            )
        } else {
            randomize_faction <- function() {
                fdata$selected_factions[sample.int(n = length(fdata$selected_factions), size = input$nplayers * input$nfactionspp)]
            }
            
            players <- get_player_names()
            assign("players", players, envir = .GlobalEnv)
            random_faction <- randomize_faction()
            
            if(input$speaker == T) {
                speaker <- sample(players, 1)
                players[players == speaker] <- paste0("**", speaker, " (speaker)", "**")
            } else {
                speaker <- NULL
            }
            
            
            if(!is.null(input$handicap)){
                handicap <- input$handicap
                beginner_factions <- fdata$ranked_factions[fdata$selected_factions %in% fdata$ranked_factions$factions,]
                beginner_factions <- beginner_factions[order(beginner_factions$ranked_factions),]
                beginner_factions <- beginner_factions[1:(length(handicap) * input$nfactionspp),]
                beginner_factions <- beginner_factions[sample.int(n = nrow(beginner_factions), size = length(handicap) * input$nfactionspp), "factions"]
            } else {
                handicap <- c()
            }
            
            assign_faction <- function(p){
                
                if(p %in% handicap){
                    y <- beginner_factions[1:input$nfactionspp]
                    x <- paste(y, collapse = ",<br>")
                    p <- paste(p, x, sep = "<br>")
                    beginner_factions <<- beginner_factions[!beginner_factions %in% y]
                    random_faction <<- random_faction[!random_faction %in% y]
                } else {
                    y <- random_faction[1:input$nfactionspp]
                    x <- paste(y, collapse = ",<br>")
                    p <- paste(p, x, sep = "<br>")
                    random_faction <<- random_faction[!random_faction %in% y]
                }
                
                return(p)
            }
            
            # Reorder to run handicap first
            players <- c(players[players %in% handicap], players[!players %in% handicap])
            
            for(p in seq_along(players)){
                players[p] <- assign_faction(p = players[p])
            }
            
            parts <- rep(1, input$nplayers)
            
            if(input$randomize_seats) names(parts) <- sample(players)
            else names(parts) <- players
            
            fdata$parts <- parts
            
            parts
        }

        

    })
    
    
    toListen <- reactive({
        list(input$nplayers, input$include_expansion, input$allfactions)
    })
    
    observeEvent(toListen(), {
        
        nf <- length(fdata$selected_factions)
        np <- input$nplayers
        nfpp <- floor(nf/np)
        
        updateNumericInput(session, "nfactionspp", "Number of factions per player", min = 1, max = nfpp, step = 1, value = nfpp)
        updateSelectInput(session, "handicap", "Prioritize 'beginner' factions for", choices = get_player_names())
        
    }, ignoreInit = T)
    
    playerListener <- reactive({
        if(is.numeric(input$nplayers)) sapply(1:input$nplayers, function(i) input[[paste0("field", i)]])
    })
    
    observeEvent(playerListener(), {
        updateSelectInput(session, "handicap", "Prioritize 'beginner' factions for", choices = get_player_names())
    })
    
    observeEvent(input$nplayers, {
        
        i = 1:input$nplayers
        updateSelectInput(session, "handicap", "Prioritize 'beginner' factions for", choices = paste0("Player ", i))
        
    }, once = T)
    
    observeEvent(input$allfactions, {
        fdata$selected_factions <<- input$allfactions
    }, ignoreInit = T)
    
    observeEvent(input$include_expansion, {
        if(input$include_expansion == T){
            updateNumericInput(session, "nplayers", "Number of players", min = 2, max = 8, step = 1)
            
            fdata$selected_factions <<- c(fdata$selected_factions, fdata$expansion_factions)
            updateCheckboxGroupInput(session, "allfactions", "Include factions", choices = c(factions, fdata$expansion_factions), inline = F, fdata$selected_factions)
            
            } else if(input$include_expansion == F) {
                if(input$nplayers >= 6) {
                    updateNumericInput(session, "nplayers", "Number of players", min = 2, max = 6, value = 6, step = 1)
                } else {
                    updateNumericInput(session, "nplayers", "Number of players", min = 2, max = 6, step = 1)
                }
                
                updateCheckboxGroupInput(session, "allfactions", "Include factions", choices = factions, inline = F, fdata$selected_factions)
                
            }
    }, ignoreInit = T)

}

enableBookmarking(store = "url")

shinyApp(ui, server)