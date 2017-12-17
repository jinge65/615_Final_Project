# loading the libraries
require(dplyr)
require(stringr)
require(shiny)
require(tm)
require(NLP)
require(RCurl)
require(stringr)
require(wordcloud)
require(wordcloud2)
require(syuzhet)

# Import all the data parts
MOE_part1 <- read.csv('Data/tweets_MOE.csv', stringsAsFactors=FALSE)
Co_part1 <- read.csv('Data/tweets_Coco.csv', stringsAsFactors=FALSE)
JL_part1 <- read.csv('Data/tweets_JL.csv', stringsAsFactors=FALSE)
DH_part1 <- read.csv('Data/tweets_DH.csv', stringsAsFactors=FALSE)
W_part1 <- read.csv('Data/tweets_W.csv', stringsAsFactors=FALSE)

MOE_part2 <- read.csv('Data/tweets_MOE2.csv', stringsAsFactors=FALSE)
Co_part2 <- read.csv('Data/tweets_Coco2.csv', stringsAsFactors=FALSE)
JL_part2 <- read.csv('Data/tweets_JL2.csv', stringsAsFactors=FALSE)
DH_part2 <- read.csv('Data/tweets_DH2.csv', stringsAsFactors=FALSE)
W_part2 <- read.csv('Data/tweets_W2.csv', stringsAsFactors=FALSE)


# Remove duplicates and combine all the data parts of different topic
MOE <- rbind(MOE_part1 %>% slice(121:n()), MOE_part2)

Coco <- rbind(Co_part1 %>% slice(1431:n()), Co_part2)

JL <- rbind(JL_part1 %>% slice(3333:n()), JL_part2)

DH <- rbind(DH_part1 %>% slice(16:n()), DH_part2)

Wonder <- rbind(W_part1 %>% slice(45:n()), W_part2)


# Add a new column called Movie_Name to each data parts
MOE <- MOE %>% mutate(MovieName = 'Murder on the Orient Express')
Coco <- Coco %>% mutate(MovieName = 'Coco')
JL <- JL %>% mutate(MovieName = 'Justice League')
DH <- DH %>% mutate(MovieName = "Daddy's Home 2")
Wonder <- Wonder %>% mutate(MovieName = 'Wonder')


# Combine all the data parts
MovieData <- rbind(MOE, Coco, JL, DH, Wonder)

# Variable Selection
MovieData <- MovieData %>%
  select(MovieName, text, favoriteCount, created, screenName, replyToSN, 
         statusSource, retweetCount, isRetweet, longitude, latitude)

# Variable Transformation
MovieData$MovieName <- as.factor(MovieData$MovieName)
MovieData$created <- as.POSIXct(MovieData$created)


# The list of valid movies
movies <- c('Murder on the Orient Express', 'Coco',
            'Justice League', "Daddy's Home 2", 'Wonder')


cleanTweets = function(movie){
  if (!(movie %in% movies))
    stop("Unknown movie")
  
  # get data
  df.tweets <- MovieData[MovieData$MovieName == movie, ]
  
  # Removes RT
  df.tweets$text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df.tweets$text)
  
  #Remove non-ASCII characters
  Encoding(df.tweets$text_clean) = "latin1"
  iconv(df.tweets$text_clean, "latin1", "ASCII", sub = "")
  
  # Removes @<twitter handle>
  df.tweets$text_clean = gsub("@\\w+", "", df.tweets$text_clean)
  # Removes punctuations
  df.tweets$text_clean = gsub("[[:punct:]]", "", df.tweets$text_clean)
  # Removes numbers
  df.tweets$text_clean = gsub("[[:digit:]]", "", df.tweets$text_clean)
  # Removes html links
  df.tweets$text_clean = gsub("http\\w+", "", df.tweets$text_clean)
  # Removes unnecessary spaces
  df.tweets$text_clean = gsub("[ \t]{2,}", "", df.tweets$text_clean)
  df.tweets$text_clean = gsub("^\\s+|\\s+$", "", df.tweets$text_clean)
  # Fix for error related to formatting 'utf8towcs'"
  df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
  return(df.tweets)
}

# Generate Term Document Matrix using stopword list from tm pacakge
tdm.tmStopWord = function(clean.tweets.dataframe){
  # Creates a text corpus from the plain text document for every tweet
  text_corpus = Corpus(VectorSource(clean.tweets.dataframe$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  # creating a Term Document Matrix 
  tdm = TermDocumentMatrix(
    # the text corpus created from the text_clean object
    text_corpus,
    # defining the stopwords to be removed before creating a term document matrix
    control = list(
      removePunctuation = TRUE,
      stopwords("en"),
      removeNumbers = TRUE,
      tolower = TRUE)
  )
  
  return(tdm)
}



# Generate Term Document Matrix without removing stopwords
tdm.tm = function(clean.tweets.dataframe){
  
  text_corpus = Corpus(VectorSource(clean.tweets.dataframe$text_clean))
  tdm = TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      tolower = TRUE))
  
  return(tdm)
}



generateWordCloud.positive.tmStopWords = function(tdm.tm.stopword){
  
  
  # converting term document matrix to matrix
  m = as.matrix(tdm.tm.stopword)
  
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing = TRUE)
  
  # create a data frame with words and their frequencies
  dm = data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lexicons = get_nrc_sentiment(as.character(dm$word))
  tweets.positive = dm[nrc.lexicons$positive>0,]
  
}

generateWordCloud.negative.tmStopWords = function(tdm.tm.stopword){
  
  # converting term document matrix to matrix
  m = as.matrix(tdm.tm.stopword)
  
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing = TRUE)
  
  # create a data frame with words and their frequencies
  dm = data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lexicons = get_nrc_sentiment(as.character(dm$word))
  
  tweets.negative = dm[nrc.lexicons$negative>0,]
}




# Define UI for application that draws a histogram
ui <- fluidPage(
  # Defining the header Panel on the shiny application 
  #h3- argument is used to obtain a specific size for the header/ title.
  #windowTitle - The title that should be displayed by the browser window. 
  headerPanel(h3("Twitter Sentiment Analysis: Movie Review"), windowTitle = "Movie Review"),
  #Sidebar Layout - used to create a layout with a sidebar and main area in the Shiny Aplication.
  sidebarLayout(
    #Create a sidebar panel containing input controls that can in turn be passed to sidebarLayout.
    #img argument is used to load an image into the sodeba panel of the shiny Application
    sidebarPanel(
      # radioButtons -Create a set of radio buttons used to select an item from a list.          
      selectInput("selection", "Choose a movie:",
                  choices = movies),
      #sliderInput -Constructs a slider widget to select a numeric value from a range.
      sliderInput("numberInput", "Select number of tweets",
                  min = 0, max = 5000, value = 150),
      hr(),
      #actionButton - Used to create a go button, that allows the shiny Application the execute the input
      actionButton("goButton", "Search", icon("twitter"),
                   style="color: #fff; background-color: #337ab7"),width = 3),
    
    #Panel to display output
    # mainPanel - Create a main panel containing output elements that can in turn be passed to sidebarLayout.
    mainPanel(
      #Tabsets - used for dividing output into multiple independently viewable sections.
      #Dividing the main panel into multiple tabs
      tabsetPanel(
        #tabPanel - Create a tab panel that can be included within a tabsetPanel.
        #Argument plotOutput - used to create a plot as an output element based on the inputid that is passed to it
        tabPanel("Sentiment Plots", plotOutput("plot1")),
        tabPanel("Positive/Negative Plots", plotOutput("plot2")),
        #navbarMenu - Creates a page that contains a top level navigation bar that can be used to toggle a set of tabPanel elements.
        navbarMenu("Word Clouds",
                   #tabPanel - Create a tab panel that can be included within a tabsetPanel
                   tabPanel("Positive", wordcloud2Output("wordCloud1", width = "100%", height = "400px")),
                   #wordcloud2Output -used to render a wordcloud object| uses library - wordcloud2
                   tabPanel("Negative", wordcloud2Output("wordCloud2", width = "100%", height = "400px"))),
        #dataTableOutput -used to render a table as an output
        tabPanel("Tweets", dataTableOutput("tweetTable"))
        ,type = "pills"), width = 9)
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session){
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data1 = reactive({
    input$goButton
    
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        #Cleans the tweet
        df.tweets <- cleanTweets(input$selection)
        # random sample of data
        df.tweets <- df.tweets[sample(nrow(df.tweets), input$numberInput), ]
        #Get sentiments
        nrc.lexicons = get_nrc_sentiment(df.tweets$text_clean)
      })
    })
  })
  # ## Rendering TM plots ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot1 and plot2
  output$plot1 = renderPlot({
    
    # ## creating Barplot for emotions ##
    barplot(
      #Sort  - (or order) a vector or factor (partially) into ascending or descending order
      #prop.table - Express Table Entries as Fraction of Marginal Table
      sort(colSums(prop.table(data1()[, 1:8]))), 
      horiz = TRUE, 
      col = 'darkred',
      cex.names = 0.8, 
      las = 1, 
      main = "Emotions in tweets", xlab="Percentage", xlim = c(0,.4))}, 
    width = 700, height = 500)
  
  output$plot2 = renderPlot({
    
    # ## Creating barplot for positive vs negative ##
    barplot(
      sort(colSums(prop.table(data1()[, 9:10]))), 
      horiz = TRUE, 
      col = 'deepskyblue3',
      cex.names = 0.75, 
      las = 1, 
      main = "Ratio of positive to negative tweets",xlab="Percentage", xlim = c(0,1))},
    width = 700, height = 500)
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data2 = reactive({
    input$goButton
    
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        df.tweets <- cleanTweets(input$selection)
        df.tweets <- df.tweets[sample(nrow(df.tweets), input$numberInput), ]
        searchtweet.tdm.tm.stopword = tdm.tmStopWord(df.tweets)
        tweets.positive = generateWordCloud.positive.tmStopWords(searchtweet.tdm.tm.stopword)
      })
    })
  })
  
  # ## Render Positive Wordcloud ##
  # renderWordcloud1 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  output$wordCloud1 = renderWordcloud2({wordcloud2(data = data2())})
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data3 = reactive({
    input$goButton
    
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        df.tweets <- cleanTweets(input$selection)
        df.tweets <- df.tweets[sample(nrow(df.tweets), input$numberInput), ]
        searchtweet.tdm.tm.stopword = tdm.tmStopWord(df.tweets)
        tweets.negative = generateWordCloud.negative.tmStopWords(searchtweet.tdm.tm.stopword)
      })
    })
  })
  
  
  # ## Render negative wordcloud ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  output$wordCloud2 = renderWordcloud2({wordcloud2(data = data3())})
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data4 = reactive({
    input$goButton
    
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        df.tweets = MovieData[MovieData$MovieName == input$selection, ]
        df.tweets <- df.tweets[sample(nrow(df.tweets), input$numberInput), ]
        
        # Remove all nongraphical characters
        text = str_replace_all(df.tweets$text,"[^[:graph:]]", " ")
        df.tweets = cbind(text, df.tweets[c(4,5,3,7,9,10)])
        
        # Changing column names
        colnames(df.tweets) = c("Tweets", "Date", "Username", "Favorites", "RT Count", "Longitude",
                                "Latitude")
        tweetOutput = df.tweets
      })
    })
  })
  
  # ##Render tweets## #
  # renderDataTable - Renders a reactive data table that is suitable for assigning to an output slot.
  # In this case the the object used is tweetTable
  output$tweetTable = renderDataTable({data4()}, options = list(lengthMenu = c(10, 30, 50), 
                                                                pageLength = 5,
                                                                searching = FALSE))
  
}

# Run the application 
shinyApp(ui = ui, server = server)