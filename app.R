
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(waffle)
library(data.table)
library(dplyr)
library(plotly)

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
    dashboardPage(
    dashboardHeader(title = "Twilight Imperium Faction Picker"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Randomize factions", tabName = "factions", icon = icon("random")),
            menuItem("Randomize tiles", tabName = "tiles", icon = icon("th")),
            menuItem("Faction info", tabName = "widgets", icon = icon("rocket"))
        )
    ),
    ## Body content
    dashboardBody(
        shinyDashboardThemes(
            theme = "grey_light"
        ),
        tabItems(
            # First tab content
            tabItem(tabName = "factions",
                    fluidRow(
                        box(width = 3,
                            title = ("Options"),
                            collapsible = T,
                            checkboxInput("include_expansion", "Include Prophecy of Kings expansion"),
                            checkboxInput("randomize_seats", "Randomize seats", value = T),
                            tags$div(title="A specific seed number will always generate the same results. If left blank, a random number will be generated.",
                            textInput("seed", "Set seed")),
                            
                            tags$div(title="How many people are playing?",
                                numericInput("nplayers", "Number of players", min = 2, max = 6, step = 1, value = 6)
                                ),
                            tags$div(title="How many factions should each player get to choose between?",
                                numericInput("nfactionspp", "Number of factions per player", min = 1, max = 2, step = 1, value = 2)
                                ),
                            checkboxGroupInput("allfactions", "Include factions", choices = factions, inline = F, selected = factions)
                            
                        ),

                        box(width = 8,
                            title = ("Player settings"),
                            collapsible = T,
                            tags$div(title="Assign player names. These fields can be left blank.",
                                wellPanel(
                                    uiOutput("ui1")
                                    )
                                ),
                            tags$div(title="Assign easier to play with factions for any player.",
                                selectInput("handicap", "Prioritize 'beginner' factions for", choices = list(), multiple = T)
                                ),
                            actionButton("assign", "Assign", icon = icon("random")),
                            bookmarkButton(id = "bookmark1", label = "Share", icon = icon("share"))
                        ),
                            
                        box(width = 8,
                            plotOutput("results")
                            
                        )
                    )
            ),
            
            tabItem(tabName = "tiles",
                    fluidRow(
                        box(width = 4,
                            title = ("Options"),
                            "Coming soon"
                        ),
                        
                        box(width = 8, height = '500px',
                            plotlyOutput("tilemap")
                        )
                        
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    h2("Faction info"),
                    "Coming soon"
            )
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
        # state is a mutable reference object, and we can add arbitrary values
        # to it.
        state$values$seed <- fdata$seed
        state$values$parts <- fdata$parts
        
    })
    onRestore(function(state) {
        fdata$seed <- state$values$seed
        updateTextInput(session, "seed", "Set seed", placeholder = as.character(fdata$seed))

        output$results <- renderPlot({
            waffle(unlist(state$values$parts), rows = 2, legend_pos = "bottom") + theme(legend.text = element_text(size = 14))
            
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
        
        if(!is.na(input$nplayers) & !is.na(input$nfactionspp)) waffle(dat(), rows = 2, legend_pos = "bottom") + theme(legend.text = element_text(size = 14))

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
            random_faction <- randomize_faction()
            
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
                    x <- paste(y, collapse = ",\n")
                    p <- paste(p, x, sep = "\n")
                    beginner_factions <<- beginner_factions[!beginner_factions %in% y]
                    random_faction <<- random_faction[!random_faction %in% y]
                } else {
                    y <- random_faction[1:input$nfactionspp]
                    x <- paste(y, collapse = ",\n")
                    p <- paste(p, x, sep = "\n")
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
    
    hexmap <- function(tiles){
        f <- ggplot(tiles, aes(x = x,
                               y = y,
                               fill = Anomaly,
                               "key" = Tile,
                               "neighbor_anomaly" = neighbor_anomaly)) +
            geom_hex(stat = "identity", alpha=0.8) +
            xlim(c(min(tiles$x - 1.5), max(tiles$x + 1.5))) +
            ylim(c(min(tiles$y - 1.5), max(tiles$y + 1.5))) +
            scale_fill_manual(values = c("forestgreen", "firebrick4", "seagreen3"), alpha) +
            theme_void()
        
        return(f)
    }
    
    makeHoneyComb <- function(rings){
        
        map <- data.table()
        map <- rbindlist(list(map, list(0, 0)))
        
        r = 0
        while(r > -rings){
            c = -r - 1
            while(c > -rings -r){
                map <- rbindlist(list(map, list(c, r)))
                c = c-1
            }
            
            r = r-1
        }
        
        r = 1
        while(r < rings){
            c = 0
            while(c > -rings){
                map <- rbindlist(list(map, list(c, r)))
                c = c-1
            }
            
            r = r+1
        }
        
        c = 1
        while(c < rings){
            r = -c
            while(r < rings - c){
                map <- rbindlist(list(map, list(c, r)))
                r = r+1
            }
            c = c+1
        }
        
        colnames(map) <- c("x", "y")
        return(map)
        
    }
    
    swap_coords <- function(tile1, tile2){
        tile1.x <- tile1[, x]
        tile1.y <- tile1[, y]
        
        tile2.x <- tile2[, x]
        tile2.y <- tile2[, y]
        
        tile1[, "x"] <- tile2.x
        tile1[, "y"] <- tile2.y
        
        tile2[, "x"] <- tile1.x
        tile2[, "y"] <- tile1.y
        
        return(list(tile1, tile2))
    }
    
    has_neighbor_anomaly <- function(tile){
        if(any(axial_neighbors(tile)$Anomaly)) return(T) else return(F)
    }
    
    shuffle <- function(type) {
        if (type == "Anomaly") return(tiles[sample(tiles[placed == F & Anomaly == T, which = T], 1)])
        else if (type == "Normal") return(tiles[sample(tiles[placed == F & Anomaly == F, which = T], 1)])
    }
    
    
    
    output$tilemap <- renderPlotly({
        
        tmp <- makeHoneyComb(4)
        
        data <- data.table(System = paste0(rep(LETTERS, each = 5), sep = "-", 1:5)[1:nrow(tmp)], colval = seq(1:nrow(tmp)), x = tmp$x, y = tmp$y)
        
        data[,"x"] <- data[,x] + data[,y] / 2
        
        axial_neighbors <- function(system) {
            r <- system[, x]
            c <- system[, y]
            
            
            data.table(tiles[(x == r + 1 & y == c) |
                                 (x == r + .5 & y == c - 1) |
                                 (x == r - .5 & y == c - 1) |
                                 (x == r -  1 & y == c) |
                                 (x == r - .5 & y == c + 1) |
                                 (x == r + .5 & y == c + 1)
                             ]
            )
        }
        
        
        tiles <- fread("~/faction_picker/tiles.csv")
        tiles$x <- 0
        tiles$y <- 0
        tiles$color <- ifelse(tiles$Anomaly, 1, 2)
        tiles$color <- as.factor(tiles$color)
        
        tiles <- tiles[13:49]
        tiles$x <- data$x
        tiles$y <- data$y
        tiles$placed <- FALSE
        tiles$neighbor_anomaly <- F
        
        
        
        
        hm <- hexmap(tiles)
        
        ggplotly(p = hm)
        
    })
    

}

enableBookmarking(store = "url")

shinyApp(ui, server)