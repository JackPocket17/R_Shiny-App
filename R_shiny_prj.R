library(shiny)
library(shiny.fluent)
library(dplyr)
library(data.table)
library(lsa)
library(tibble)
library(DT)
library(plotly)
library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(highcharter)
library(RColorBrewer)
library(htmlwidgets)

data = read.csv('SpotifyFeatures.csv')

test.env <- new.env()

columns <- list(
  list(key = "track_name", fieldName = "track_name", name = "Track Name"),
  list(key = "artist_name", fieldName = "artist_name", name = "Artist Name"),
  list(key = "genre", fieldName = "genre", name = "Genre")
)

Card <- function(..., title = NULL) {
  div(
    class = "ms-depth-8",
    Stack(
      tokens = list(padding = 20, childrenGap=10),
      if (!is.null(title)) {Text(title, variant = "large")},  
      ...
    )
  )
}

Grid <- function(...) {
  div(
    class = 'ms-Grid', dir = "ltr",
    style = "padding: 0px",
    ...
  )
}

GridItem <- function(..., class = "ms-sm12") {
  div(
    class = paste("ms-Grid-col", class),
    style = "padding: 10px",
    ...
  )
}

ui <- fluentPage(
  Pivot(
    PivotItem(headerText = "Home Page", 
      Grid(
        GridItem(class = "ms-sm4 ms-xl4",
                 Card(title = "Inputs",
                      Text(" "),
                      TextField.shinyInput("no_playlist", 
                                           label="No. of playlists", 
                                           value="3",
                                           placeholder="Number of playlists"),
                      TextField.shinyInput("no_song_playlist", 
                                           label="No. of songs per playlist", 
                                           value="10", 
                                           placeholder="Number of songs per playlist"),
                      br(),
                      Slider.shinyInput("input1", label="Popularity", min= 0, max=100),
                      Slider.shinyInput("input2", label="Dance",min=0,  max=1, step = 0.0001),
                      Slider.shinyInput("input3", label="Accousticness", min=0,  max=1, step = 0.0001),
                      Slider.shinyInput("input4", label="Energy", min=0,  max=1, step = 0.0001),
                      br(),
                      PrimaryButton.shinyInput("submit", text = "Generate Playlists"),
                      DefaultButton.shinyInput("clear", text = "Clear Results", styles = list("background: green")),
                 ),
              ),
        
        GridItem(class = "ms-sm8",
                 Card(title = "Output Playlists", 
                      lapply(1:10, function(i) {
                        uiOutput(paste0('table', i))
                      })
                 )
        ),
    ), linkSize='large'),
    PivotItem(headerText = "Analytics", Grid(
      GridItem(class = "ms-sm8",
               Card(
                 title="Cluster plot",
                 plotlyOutput("cplot")
               )),
      GridItem(class = "ms-sm12",
               Card(title = "Plots",
                    Text(" "),
                    lapply(1:10, function(i) {
                        uiOutput(paste0('plot', i))
                    })
               ),
      ),
    ))
  ),
  
)

server <- function(input, output, session) {
  
  lapply(1:10, function(i) {
    output[[paste0('table', i)]] <- renderUI({
      Text("")
    })
    output[[paste0('plot', i)]] <- renderUI({
      Text("")
    })
  })
  
  observeEvent(input$submit, {

    no_of_playlist = as.integer(input$no_playlist)
    songs_per_playlist = as.integer(input$no_song_playlist)
    total = no_of_playlist * songs_per_playlist
    inputs = c(input$input1,input$input2, input$input3, input$input4)
    
    final_features = c('popularity', 'danceability', 'acousticness', 'energy')
    new_data = data[,final_features] # Getting our target similarity columns
    
    similarity <- function(x) {
      return (cosine(x, inputs))
    }
    
    ss <- apply(new_data, 1, similarity)
    new_data <- cbind(new_data, similar = ss)
    ordered <- new_data[order(-new_data$similar),]
    subset = ordered[1:total,]
    
    subset <- subset[,final_features]
    
    # Performing clustering and assigning cluster number to our data
    km.res <- kmeans(subset, no_of_playlist)
    subset <- cbind(subset, cluster = as.vector(km.res$cluster))
    
    # Splitting the dataset based on cluster number
    X <- split(subset, subset$cluster)
    
    table_features = c('popularity', 'danceability', 'acousticness', 'energy' , 'artist_name', "track_name" )

    song_name = c('artist_name', "track_name" )
    data_names = data[, song_name]

    data_songs_merge = cbind(data_names, new_data)

    ordered_ <- data_songs_merge[order(-data_songs_merge$similar),]
    subset_songs = ordered_[1:total,]

    artist_song_name = subset_songs[,song_name]

    sub_subset_songs = subset_songs[,final_features]

    km.res_songs <- kmeans(sub_subset_songs, no_of_playlist)
    sub_subset_songs <- cbind(sub_subset_songs, cluster = as.vector(km.res_songs$cluster),artist_song_name)
    sub_subset_songs_plot <- subset(sub_subset_songs, select = c("popularity","danceability","acousticness","energy", "cluster" ))

    plot_songs =  fviz_cluster(km.res_songs, 
                               data = sub_subset_songs_plot,
                               geom=c("point"),
                               ellipse.type="euclid",
                               main=" ", xlab = " ", ylab = " ")

    output$cplot <- renderPlotly(plot_songs)
    
    lapply(1:no_of_playlist, function(i) {
      # output[[paste0('table', i)]] <- renderTable({data[rownames(data.frame(X[i])),c('artist_name', 'track_name', 'track_id', 'duration_ms')]})
      dd <- data[rownames(data.frame(X[i])),]
      aa <- data[rownames(data.frame(X[i])),c('track_name', 'artist_name', 'genre')]
      output[[paste0('table', i)]] <- renderUI({
        Card(title=paste("Playlist",i),
             Text(paste("No. of songs:",nrow(aa))),
             DetailsList(items = aa, columns = columns, checkboxVisibility = FALSE,
                         cellsByColumn=10,
                         disableSelectionZone = TRUE)
        )
             
      })
      
      j = 1
      
      output[[paste0('plot', i)]] <- renderUI({
        a <- data[rownames(data.frame(X[i])),]
        progress <- shiny::Progress$new()
        progress$set(message = paste("Computing data for playlist", i), value = 0)
        on.exit(progress$close())
        features = c("danceability", "energy", "loudness", "speechiness", "acousticness",
                     "instrumentalness", "liveness", "valence", "tempo")
        # am <- map(features, function(x){
        #   a[[x]] %>%
        #     hchart(showInLegend = FALSE) %>%
        #     hc_title(text = "Playlist")
        #     hc_add_theme(hc_theme_smpl()) %>%
        #     hc_title(text = x) %>%
        #     hc_yAxis(title = list(text = ""))
        # }) %>%
        #   hw_grid(rowheight = 150, ncol = 9)
        # Card(
        #   List(
        #     items = 
        #   )
        # )
        
       list(Text(paste(paste("Playlist", i),"analysis:")),
         map(features, function(x){
         progress$inc(1/j, detail = paste("Doing part", j))
         j = j+1
         a[[x]] %>%
           hchart(showInLegend = FALSE) %>%
           # hc_title(text = "Playlist") %>%
           hc_add_theme(hc_theme_smpl()) %>%
           # hc_title(text = x) %>%
           hc_xAxis(title = list(text = x)) %>%
           hc_yAxis(title = list(text = ""))
       }) %>%
         hw_grid(rowheight = 150, ncol = 9))
      })
    })

  })
  
  observeEvent(input$clear, {
    lapply(1:10, function(i) {
      output[[paste0('table', i)]] <- renderUI({
        Text(" ")
      })
      output[[paste0('plot', i)]] <- renderUI({
        Text(" ")
      })
    })
    output$cplot <- renderPlotly({
      
    })
  })
}

shinyApp(ui, server)
