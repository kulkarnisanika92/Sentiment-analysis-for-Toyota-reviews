# Function to check if packages are installed. Install if they are not.
list.of.packages <- c("shiny", "rvest", "DT", "stringr", "gsubfn", "tidyr","tidytext","dplyr",
                      "ggplot2","shinydashboard","rpart","shinycssloaders","tm","colorspace")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Import libraries
library(shiny)
library(rvest)
library(DT)
library(stringr)
library(gsubfn)
library(tidyr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(rpart)
library(shinycssloaders)
library(tm)
library(colorspace)


ui <- dashboardPage(
  
  dashboardHeader(title = "Sanika_Kulkarni_NLP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Actual Train Data", tabName = "TrainData"),
      menuItem("Actual Test Data", tabName = "TestData"),
      menuItem("Normalized Train Data", tabName = "NormalizedTrainData"),
      menuItem("Normalized Test Data", tabName = "NormalizedTestData"),
      menuItem("Sentiment Analysis for Train dataset", tabName = "SentimentAnalysisTrain"),
      menuItem("Sentiment Analysis for Test dataset", tabName = "SentimentAnalysisTest"),
      menuItem("Comparison", tabName = "Comparison"),
      menuItem("Model Summary", tabName = "ModelSummary"),
      menuItem("Model Accuracy", tabName = "ModelAccuracy"),
      menuItem("TF-IDF with Stop words", tabName = "TF-IDF_with"),
      menuItem("TF-IDF without Stop words", tabName = "TF-IDF_without"),
      menuItem("TF-IDF Visualization", tabName = "TF-IDF_Vis")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "TrainData",
              fluidRow(box(title = "Actual Train Data",width=12, shiny :: dataTableOutput("table1") %>% withSpinner(color="#0dc5c1")))
      ),
      tabItem(tabName = "TestData",
              fluidRow(box(title = "Actual Test Data",width=12, shiny :: dataTableOutput("table2") %>% withSpinner(color="#0dc5c1")))
              
      ),
      tabItem(tabName = "NormalizedTrainData",
              fluidRow(box(title = "Normalized Train Data",width=12, shiny :: dataTableOutput("table3") %>% withSpinner(color="#0dc5c1")))
      ),
      tabItem(tabName = "NormalizedTestData",
              fluidRow(box(title = "Normalized Test Data", width = 12, shiny :: dataTableOutput("table4") %>% withSpinner(color="#0dc5c1")))),
      tabItem(tabName = "SentimentAnalysisTrain",
              fluidRow(box(title = "Sentiment Analysis for Train dataset", width = 12, shiny :: dataTableOutput("table5") %>% withSpinner(color="#0dc5c1")))
      ),
      tabItem(tabName = "SentimentAnalysisTest",
              fluidRow(box(title = "Sentiment Analysis for Test dataset", width = 12, shiny :: dataTableOutput("table6")%>% withSpinner(color="#0dc5c1")))),
      tabItem(tabName = "Comparison",
              fluidRow(
                column(width = 6,
                       box(title = "Comparison between training and User", width = 12, shiny :: tableOutput("table7")%>% withSpinner(color="#0dc5c1"))),
                column(width = 6,
                       box(title = "Average sentiment analysis for each tag before scaling", width = 12, shiny :: tableOutput("table8")%>% withSpinner(color="#0dc5c1") )),
                column(width = 6,
                       box(title = "Average sentiment analysis for each tag after scaling", width = 12, shiny :: tableOutput("table14")%>% withSpinner(color="#0dc5c1") )),
                
                h4("The average sentiment rating is scaled on the basis from 1 to 5 based on afinn score. As afinn score is not calculated considering n grams, 
                   there is small difference in average rating calculated by sentiment analysis and given by user"),
                h4("As 4 tags don't represent all reviews in training dataset, there is small difference in average sentiment rating corresponding 
                   to each 4 tags, respect to average star rating provided by user and average sentiment rating for all reviews from training dataset")
                )
      ),
      tabItem(tabName = "ModelSummary",
              fluidRow(box(title = "Summary of Linear Regression Model", width = 12, shiny :: verbatimTextOutput("summary") %>% withSpinner(color="#0dc5c1"))),
              h4("Stars besides coefficient explain statistical significance. So afinn score is highly statistically significant."),
              h4("Lower p value indicate, afinn score has more impact on dependent variable - rating.")
              ),
            
      tabItem(tabName = "ModelAccuracy",
              fluidRow(
                column(width = 8,
                box(title = "Ratings based on Prediction Model", width = 12, shiny :: dataTableOutput("table10")%>% withSpinner(color="#0dc5c1"))),
                column(width = 4,
                       fluidRow(box(title = "Min-Max Accuracy of the Prediction Model", width = 12, shiny :: verbatimTextOutput("min_max_accuracy")%>% withSpinner(color="#0dc5c1"))),
                       fluidRow(box(title = "Mean Absolute Percentage Deviation of the Prediction Model", width = 12, shiny :: verbatimTextOutput("mapd_accuracy")%>% withSpinner(color="#0dc5c1"))),
                       fluidRow(box(title = "Confusion Matrix", width = 12, shiny :: tableOutput("table13")))
                       
                       ))),
      tabItem(tabName = "TF-IDF_with",
              fluidRow(box(title = "TF-IDF_with stopwords", width = 12, shiny :: dataTableOutput("table11") %>% withSpinner(color="#0dc5c1")))),
      tabItem(tabName = "TF-IDF_without",
              fluidRow(box(title = "TF-IDF_without stopwords", width = 12, shiny :: dataTableOutput("table12") %>% withSpinner(color="#0dc5c1")))),
      tabItem(tabName = "TF-IDF_Vis",
              fluidRow(box(title = "TF-IDF Visualization", width = 12, shiny :: plotOutput("plot2") %>% withSpinner(color="#0dc5c1"))))
      
    )
  )
 
)

server <- function(input, output) {
  
  # Function to scrap web data
  review_data <- function(review_year){
    reviews_all <- NULL
    for(year in review_year)
    {
      url_data <- paste0("https://www.cars.com/research/toyota-camry-",year,"/consumer-reviews/?nr=1000&pg=1")
      url <- read_html(url_data)
      reviewDesc <- url %>% html_nodes('.mmy-reviews__blurb') %>% html_text() %>% str_trim() %>% unlist()
      rating <- url %>% html_nodes('.cr-star-rating') %>% html_attr("rating")
      yearList <- rep(year, length(reviewDesc))
      reviews_all <- rbind(reviews_all, cbind(yearList, reviewDesc, rating))
      }
    
    return(data.frame(reviews_all)) 
    
  }
  
  # training dataset stored in reviewTraining variable
    year <- c(2012,2013,2014,2015,2016)
    reviewTraining <- as.data.frame(review_data(review_year = year))
  
  # testing dataset stored in reviewTesting variable
    reviewTesting <- as.data.frame(review_data(review_year = 2017))
  
  
  
  # Display actual training data without normalization
  output$table1 <- shiny :: renderDataTable({
    Sys.sleep(0.3)
    actual_train_data <- reviewTraining
    colnames(actual_train_data) <- c("Year","Review Description","Rating")
    return(as.data.frame(actual_train_data))
  })
  
  # Display actual testing dataset for year 2017 without normalization
  output$table2 <- shiny :: renderDataTable({
    Sys.sleep(0.1)
    actual_test_data <- reviewTesting
    colnames(actual_test_data) <- c("Year","Review Description","Rating")
    return(as.data.frame(actual_test_data))
  })
  
  
  #function for normalized review
  normalized_data <- function(review_data){
    review_data$ID <- seq.int(nrow(review_data))
    # convert to lower case
    review_data$normalizedDesc <- tolower(review_data$reviewDesc)
    # remove punctuation
    review_data$normalizedDesc <- gsub("[[:punct:]]", "", review_data$normalizedDesc)
    # check for tags
    reviewTag1 <- ifelse(grepl("service", review_data$normalizedDesc), "service", "")
    reviewTag2 <- ifelse(grepl("price", review_data$normalizedDesc), "price","")
    reviewTag3 <- ifelse(grepl("handling", review_data$normalizedDesc),"handling","")
    reviewTag4 <- ifelse(grepl("interior", review_data$normalizedDesc),"interior","")
    revTag <-  as.data.frame(cbind(reviewTag1, reviewTag2,reviewTag3,reviewTag4))
    a <- unite(revTag,"ReviewTag",sep =" ", remove = TRUE)
    normalizedReview <- as.data.frame(cbind(review_data,a))
    normalizedReview$ID <- NULL
    
    # return normalized review - lower case, tag, punctuation removal
    return(data.frame(normalizedReview))
  }
  
    # stored normalized training data in normalizedTrainingData variable
    normalizedTrainingData <- as.data.frame(normalized_data(review_data = reviewTraining))
    
    # stored normalized test data in normalizedTestData variable
    normalizedTestData <- as.data.frame(normalized_data(review_data = reviewTesting))
    

  
  # Display normalized review for training dataset
  output$table3 <- shiny :: renderDataTable({
    Sys.sleep(0.1)
    normalized_training <- normalizedTrainingData
    colnames(normalized_training) <- c("Year","Review Description","Rating","Normalized Review","Tag")
    return(normalized_training)
  })
  
  
  # Display normalized review for testing dataset
  output$table4 <- shiny :: renderDataTable({
    Sys.sleep(0.1)
    normalized_test <- normalizedTestData
    colnames(normalized_test) <- c("Year","Review Description","Rating","Normalized Review","Tag")
    return(normalized_test)
  })
  
  
  #Function for computing sentiment analysis
  Sentiment_data <- function(normalizedReview){
   
    normalizedReview$ID <- seq.int(nrow(normalizedReview))
    #normalizedReview$normalizedDesc <- removeWords(normalizedReview$normalizedDesc, stopwords("english"))
    # removed digits from normalized review
    normalizedReview$normalizedDesc <- gsub("[[:digit:]]","",normalizedReview$normalizedDesc)
    # normalized review converted to tokens and removed stopwords
    tokens <- normalizedReview %>% unnest_tokens(word,normalizedDesc) %>% anti_join(stop_words)
    # calculated sentiment analysis for tokens using "afinn" lexicon and summed that score for each review
    sentiment <- tokens %>% inner_join(get_sentiments("afinn")) %>%
      group_by(ID)%>% summarize(afinn = sum(score, na.rm = TRUE))
    # calculated average sentiment score by taking average of score
    AvgSentiment <- tokens %>% inner_join(get_sentiments("afinn")) %>%
      group_by(ID)%>% summarize(afinn = mean(score, na.rm = TRUE))
    # scaled average score to range from 1 to 5
    AvgSentiment$ScaleAfinn <- scales :: rescale(AvgSentiment$afinn, to = c(1,5))
    AvgSentiment$afinn <- NULL
    Sentiment_data <- merge(normalizedReview,sentiment, by = "ID")
    Sentiment_data <- merge(Sentiment_data, AvgSentiment, by = "ID")
    Sentiment_data$ID <- NULL
    Sentiment_data$reviewDesc <- NULL
    return(data.frame(Sentiment_data))
  }
  
  # Sentiment analysis for training dataset
  output$table5 <- shiny :: renderDataTable({
    Sys.sleep(0.1)
    sentiment_data_train <- Sentiment_data(normalizedTrainingData)
    sentiment_data_train$ScaleAfinn <- round(sentiment_data_train$ScaleAfinn, digits = 2)
    colnames(sentiment_data_train) <- c("Year","Rating","Normalized Review","Tag","Afinn score(sum)","Scale Afinn score(avg)")
    return(sentiment_data_train)
  })
  
  
  # Sentiment analysis for testing dataset
  output$table6 <- shiny :: renderDataTable({
    Sys.sleep(0.1)
    sentiment_data_test <- Sentiment_data(normalizedTestData)
    sentiment_data_test$ScaleAfinn <- round(sentiment_data_test$ScaleAfinn, digits = 2)
    colnames(sentiment_data_test) <- c("Year","Rating","Normalized Review","Tag","Avg Afinn score(sum)","Scale Afinn score(avg)")
    return(sentiment_data_test)
  })
  
  # Comparison with sentiment analysis and user rating
  output$table7 <- shiny :: renderTable({
    sentiment_data_train <- Sentiment_data(normalizedTrainingData)
    # calculated average rating provided by user
    Avg_byUser <- sentiment_data_train %>% summarize(Name = "Avg Rating by User",AvgSentimentRating = mean(as.numeric(levels(rating))[rating]))
    # calculated average sentiment rating by average of afinn score
    Avg_byAfinn <- sentiment_data_train %>% summarize(Name = "Avg Sentiment Rating in Training Set",AvgSentimentRating = mean(afinn))
    # Calculated average sentiment rating after scaling sentiment score
    Avg_byScaleAfinn <- sentiment_data_train %>% summarize(Name = "Avg scaled Sentiment Rating in Training Set", AvgSentimentRating = mean(ScaleAfinn))
    Avg_sentiment <- rbind.data.frame(Avg_byAfinn,Avg_byUser, Avg_byScaleAfinn)
    return(Avg_sentiment)
  })
  
  # Average Sentiment analysis for each tag
  output$table8 <- shiny :: renderTable({
    sentiment_data_train <- Sentiment_data(normalizedTrainingData)
    # calculated average sentiment rating for each tag before scaling
    Avg_byTagInt <- sentiment_data_train[grepl('interior',sentiment_data_train$ReviewTag),] %>% summarize(TagName = "interior", AvgByTag = mean(afinn))
    Avg_byTagHan <- sentiment_data_train[grepl('handling', sentiment_data_train$ReviewTag),] %>% summarize(TagName = "handling", AvgByTag = mean(afinn))
    Avg_byTagSer <- sentiment_data_train[grepl('service', sentiment_data_train$ReviewTag),] %>% summarize(TagName = "service", AvgByTag = mean(afinn))
    Avg_byTagPrice <- sentiment_data_train[grepl('price', sentiment_data_train$ReviewTag),] %>% summarize(TagName = "price", AvgByTag = mean(afinn))
    Avg_allTag <- rbind.data.frame(Avg_byTagHan,Avg_byTagInt,Avg_byTagPrice,Avg_byTagSer)
    return(Avg_allTag)
  })
  
  
  output$table14 <- shiny :: renderTable({
    sentiment_data_train <- Sentiment_data(normalizedTrainingData)
    # calculated average sentiment rating for each tag after scaling
    Avg_byTagInt <- sentiment_data_train[grepl('interior',sentiment_data_train$ReviewTag),] %>% summarize(TagName = "interior", AvgByTag = mean(ScaleAfinn))
    Avg_byTagHan <- sentiment_data_train[grepl('handling', sentiment_data_train$ReviewTag),] %>% summarize(TagName = "handling", AvgByTag = mean(ScaleAfinn))
    Avg_byTagSer <- sentiment_data_train[grepl('service', sentiment_data_train$ReviewTag),] %>% summarize(TagName = "service", AvgByTag = mean(ScaleAfinn))
    Avg_byTagPrice <- sentiment_data_train[grepl('price', sentiment_data_train$ReviewTag),] %>% summarize(TagName = "price", AvgByTag = mean(ScaleAfinn))
    AvgScale_allTag <- rbind.data.frame(Avg_byTagHan,Avg_byTagInt,Avg_byTagPrice,Avg_byTagSer)
    return(AvgScale_allTag)
  })
  
 
  # function to build model 
  
  model_building <- function(model_data){

    model_data$ID <- seq.int(nrow(model_data))
    model_data$normalizedDesc <- gsub("[[:digit:]]","",model_data$normalizedDesc)
    #model_data$normalizedDesc <- removeWords(model_data$normalizedDesc, stopwords("english"))
    tokens <- model_data %>% unnest_tokens(word,normalizedDesc) %>% anti_join(stop_words)
    sentiment <- tokens %>% inner_join(get_sentiments("afinn")) %>%
      group_by(ID)%>% summarize(afinn = mean(score, na.rm = TRUE))
    sentiment$afinn <- scales :: rescale(sentiment$afinn, to = c(1,5))
    model_data <- merge(model_data,sentiment, by = "ID")
    model_data$rating <- as.numeric(as.character(model_data$rating))
    model_data <- model_data[,c("normalizedDesc","rating","afinn")]
    return(data.frame(model_data))
  }
  

  model_training_data <- as.data.frame(model_building(model_data = normalizedTrainingData))
  # used linear regression for model building
  regressor <- lm(formula = rating ~ afinn, data = model_training_data)
  model_test_data <- as.data.frame(model_building(model_data = normalizedTestData))
  # predicted values for testing dataset
  y_pred <- predict(regressor, newdata = model_test_data)
  # As rating value will range from 1 to 5, round of predicted rating to compare with actual one
  y_pred <- round(y_pred, digits = 0)
  model_test_data <- cbind(model_test_data,y_pred)
  
  # Summary of model based on training data
  output$summary <- shiny :: renderPrint({
    summary(regressor)
  })
  
 # Displayed predicted values based on model
  output$table10 <- shiny :: renderDataTable({
    prediction_test <- model_test_data[,c("normalizedDesc","rating","y_pred")]
    colnames(prediction_test) <- c("Normalized Review","Rating","Predicted Rating")
    return(prediction_test)
   # min_max_accuracy <- mean(apply(actual_pred, 1, min) / apply(actual_pred, 1, max))
  })
  
  actual_pred <- data.frame(cbind(actuals = model_test_data$rating, predicteds=model_test_data$y_pred))
  
  # calculated accuracy of the model
  output$min_max_accuracy <- shiny :: renderPrint({
    min_max_accuracy <- mean(apply(actual_pred, 1, min) / apply(actual_pred, 1, max))
    return(min_max_accuracy)
  })
  
  output$mapd_accuracy <- shiny :: renderPrint({
    mape <- mean(abs((actual_pred$predicteds - actual_pred$actuals))/actual_pred$actuals)
    return(mape)
  })
  
  # calculated correlation matrix
  output$table13 <- shiny :: renderTable({
    correlation_accuracy <- cor(actual_pred)
    return(correlation_accuracy)
  })
  
  # Function to compute tokenzation
  tokenization <- function(){
    tokenizedData <- normalizedTrainingData
    tokenizedData$normalizedDesc <- gsub("[[:digit:]]","",tokenizedData$normalizedDesc)
    tokenizedData$TagInt <- ifelse(grepl("interior",tokenizedData$ReviewTag),"interior","")
    tokenizedData$tagHand <- ifelse(grepl("handling", tokenizedData$ReviewTag),"handling","")
    tokenizedData$tagSer <- ifelse(grepl("service", tokenizedData$ReviewTag),"service","")
    tokenizedData$tagPrice <- ifelse(grepl("price", tokenizedData$ReviewTag),"price","")
    interior <- subset(tokenizedData, TagInt %in% "interior")
    interior$Tag <- "interior"
    interior <- interior[,c("normalizedDesc","Tag")]
    handling <- subset(tokenizedData, tagHand %in% "handling")
    handling$Tag <- "handling"
    handling <- handling[,c("normalizedDesc","Tag")]
    service <- subset(tokenizedData, tagSer %in% "service")
    service$Tag <- "service"
    service <- service[,c("normalizedDesc","Tag")]
    price <- subset(tokenizedData, tagPrice %in% "price")
    price$Tag <- "price"
    price <- price[,c("normalizedDesc","Tag")]
    
    tokenizedData <- rbind(interior,handling,service,price)
    
    tokenizedData <- tokenizedData %>%
      unnest_tokens(word, normalizedDesc) %>%
      count(Tag, word, sort = TRUE) %>%
      ungroup()
    
    
    colnames(tokenizedData) <- c("Tag","word","n")
    
    return(data.frame(tokenizedData))
  }
  
  # TF-IDF score without removing stop words
  output$table11 <- shiny :: renderDataTable({
    Sys.sleep(0.1)
    termFre_vis_withStopwords <- as.data.frame(tokenization())
    # calculate tf-idf
    review_words <- termFre_vis_withStopwords %>% bind_tf_idf(word, Tag, n)
    return(as.data.frame(review_words))
  })
  
  
  # TF-IDF score after removing stopwords
  output$table12 <- shiny :: renderDataTable({
    Sys.sleep(0.1)
    termFre_vis_withoutStopwords <- as.data.frame(tokenization())
    # calculate tf-idf
    review_words <- termFre_vis_withoutStopwords  %>% bind_tf_idf(word, Tag, n) %>% anti_join(stop_words)
    return(as.data.frame(review_words))
  })
  
  
  
  # Visualized TF-IDF scores for top 10 words for every tag
  output$plot2 <- shiny :: renderPlot({
    termFre_vis <- as.data.frame(tokenization())
    termFre_vis <- termFre_vis %>%
      bind_tf_idf(word, Tag, n) %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word))))
    
    visplot <- termFre_vis %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>% 
      group_by(Tag) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = Tag)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~Tag, ncol = 2, scales = "free") +
      coord_flip()
    
    plot(visplot)
   
  })
  
  
}

shinyApp(ui = ui, server = server)
