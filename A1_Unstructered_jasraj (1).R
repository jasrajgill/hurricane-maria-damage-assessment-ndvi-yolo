#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)
library(dplyr)
library(ggplot2)
library(tidytext)
library(textdata)
library(tidyr)
library(igraph)
library(ggraph)
library(stringr)
library(janeaustenr)
library(scales)
library(GGally)
library(tm)
library(slam)
library(topicmodels)
library(wordcloud)
library(shiny)
library(shinydashboard)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://jasraj0898:Pikachu@cluster0.umnqiqy.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find() %>% 
              as_tibble() # Convert to a tibble for easier manipulation with dplyr

# Dropping columns irrelevant to analysis
airbnb <- airbnb_all %>% select(-listing_url, -last_scraped, -calendar_last_scraped, -weekly_price, 
                                -monthly_price, -images, -host, -address,-availability, -review_scores, -reviews,
                                -first_review, -last_review, -reviews_per_month)

# Checking null values
colSums(is.na(airbnb))

# Handle missing values
listings_data <- airbnb %>%
  mutate(
    bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm = TRUE), bedrooms),
    beds = ifelse(is.na(beds), median(beds, na.rm = TRUE), beds),
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = TRUE), bathrooms),
    cleaning_fee = ifelse(is.na(cleaning_fee), median(cleaning_fee, na.rm = TRUE), cleaning_fee),
    security_deposit = ifelse(is.na(security_deposit), median(security_deposit, na.rm = TRUE), security_deposit)
  )

# changing the given columns into integers
listings_data <- listings_data %>%
  mutate(
    minimum_nights = as.integer(minimum_nights),
    maximum_nights = as.integer(maximum_nights)
  )

# Let's understand the given data 
# Distribution of numeric variables
listings_data %>%
  select_if(is.numeric) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) + facet_wrap(~variable, scales = "free") +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() + labs(title = "Distribution of Numeric Variables")

# Distribution of categorical variables
listings_data %>%
  select('property_type', 'room_type', 'bed_type', 'cancellation_policy') %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_bar(fill = "coral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Categorical Variables")

#The basic analysis is done. Now let's explore the relationship of price with different features further.

# Visualization 1: Price Distribution by Room and Property Type
# Note: Because there might be many property types, we'll only focus on the most common ones.

# Determine the top 5 property types by frequency
top_property_types <- listings_data %>%
  count(property_type, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(property_type)

# Filter the dataframe to include only the top 5 property types
listings_data_top5 <- listings_data %>%
  filter(property_type %in% top_property_types)

# Now, plot the price distribution for these top 5 property types
plot_price_room_type <- ggplot(listings_data_top5, aes(x = room_type, y = price, fill = property_type)) +
                        geom_boxplot(outlier.shape = NA) + # Optionally hide outliers
                        coord_cartesian(ylim = c(0, quantile(listings_data_top5$price, 0.95))) + # Limiting the y-axis to 95th percentile
                        scale_y_continuous(labels = comma) +
                        labs(title = "Price Distribution by Room and Top 5 Property Types",
                             x = "Room Type",
                             y = "Price (Limited to 95th percentile)") +
                        theme_minimal() +
                        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
plot_price_room_type

# Visualization 2: Cancellation Policy Impact on Pricing
plot_cancellation_policy_price <- ggplot(listings_data, aes(x = cancellation_policy, y = price, fill = cancellation_policy)) +
                                  geom_violin(trim = FALSE) +
                                  scale_y_log10(labels = comma) + # Log transformation of the y-axis
                                  labs(title = "Cancellation Policy and Price Distribution (Log Scale)",
                                       x = "Cancellation Policy",
                                       y = "Price (Log Scale)") +
                                  theme_minimal() +
                                  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
plot_cancellation_policy_price

# Note: In these scripts, scale_y_continuous is used with labels = scales::comma to format the y-axis values with commas for better readability.

# Sample amenities string format: '["Wifi", "TV", "Kitchen", ...]'
# Let's clean it up and split into individual amenities

listings_data <- listings_data %>%
  mutate(amenities = str_replace_all(amenities, '[\\[\\]""]', ''), # Remove brackets and double quotes
         amenities = str_split(amenities, ',')) %>% # Split amenities into a list
  unnest(amenities) %>% # Convert the list column into a long format
  mutate(amenities = str_trim(amenities)) # Trim any leading or trailing whitespace

# Now we can calculate the frequency of each amenity
amenities_count <- listings_data %>%
  count(amenities, sort = TRUE)

# Top 10 most common amenities
top_amenities <- head(amenities_count, 10)

# We could also visualize this data
# Visualization 3: Top 10 Most Common Amenities
plot_common_amenities <- ggplot(top_amenities, aes(x = reorder(amenities, n), y = n)) +
                          geom_bar(stat = "identity") +
                          coord_flip() + # Flip coordinates for horizontal bar chart
                          labs(title = "Top 10 Most Common Amenities",
                               x = "Amenities",
                               y = "Frequency") +
                          theme_minimal()
plot_common_amenities

# Visualization 4: Scatter Plot of Number of Reviews and Price (Log Scale)
# Applying a log transformation to price for a more uniform distribution and better visibility
# Also, adding a trendline to visualize the general trend
plot_scatter_reviews_price <- ggplot(listings_data, aes(x = number_of_reviews, y = price)) +
                                geom_point(alpha = 0.5, color = "darkorange") +  # Use alpha to make overplotting less intense
                                geom_smooth(method = "lm", se = FALSE, color = "dodgerblue") +  # Add a linear model trend line
                                scale_y_log10(labels = scales::dollar) +  # Log transform the y-axis and format it as currency
                                scale_x_continuous(limits = c(0, quantile(listings_data$number_of_reviews, 0.95)),  # Limit x-axis to 95th percentile
                                                   breaks = scales::pretty_breaks(n = 10)) +  # Use pretty breaks for x axis
                                labs(title = "Scatter Plot of Number of Reviews and Price (Log Scale)",
                                     x = "Number of Reviews",
                                     y = "Price (Log Scale)") +
                                theme_minimal() +
                                theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_scatter_reviews_price
# Let's Analyze the text data

# Ensure 'description' is a character vector
listings_data <- listings_data %>%
  mutate(description = as.character(description), id = row_number())

# Unnest tokens to words
words <- listings_data %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word")

# Sentiment analysis with all three lexicons
afinn <- words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(afinn_score = sum(value)) %>%
  mutate(method = 'AFINN')

nrc <- words %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(id) %>%
  summarize(nrc_score = sum(sentiment == "positive") - sum(sentiment == "negative")) %>%
  mutate(method = 'NRC')

bing <- words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(id) %>%
  summarize(bing_score = sum(sentiment == "positive") - sum(sentiment == "negative")) %>%
  mutate(method = 'Bing')

# Join all sentiment data frames by 'id' and select the 'sentiment_score' and 'price' columns
sentiment_data <- afinn %>%
  inner_join(nrc, by = "id", suffix = c("_afinn", "_nrc")) %>%
  inner_join(bing, by = "id") %>%
  inner_join(listings_data %>% select(id, price), by = 'id')%>%
  select(id, afinn_score, nrc_score, bing_score, price) %>%
  # Log-transform price for better visualization
  mutate(log_price = log(price)+1)

# Visualization 5: Create a correlogram
plot_correlogram <- ggpairs(sentiment_data, 
                    columns = c("afinn_score", "nrc_score", "bing_score", "log_price"), 
                    title = "Correlogram of Sentiment Scores and Log(Price)") +
                    theme_minimal()
plot_correlogram
# LDA Analysis

# Create a tidy text data frame for neighborhood_overview column
tidy_text <- listings_data %>%
  unnest_tokens(word, neighborhood_overview) %>%
  anti_join(stop_words, by = "word")

# casting a dtm
dtm <- tidy_text %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

# Fit LDA model on the DTM
lda_model <- LDA(dtm, k = 5)  # k is the number of topics

# Convert LDA model output to a tidy format
topics <- tidy(lda_model, matrix = "beta")

# Get the top terms for each topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Helper function to reorder within facets
reorder_within <- function(x, by, within, fun = mean) {
  factor(x, levels = rev(unique(x[order(by)])), ordered = TRUE)
}

# Helper function to clean up the reorder_within for the plot
scale_x_reordered <- function() {
  scale_x_discrete(limits = rev)
}

# Visualization 6: Visualize the terms associated with each topic
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = as.factor(topic))) +
            geom_bar(stat = "identity", show.legend = FALSE) +
            facet_wrap(~ topic, scales = "free") +
            coord_flip() +
            labs(title = "Top Terms in Each Topic from LDA",
                 x = "Terms",
                 y = "Beta") +
            theme_minimal() +
            scale_x_reordered()

# N-grams

# Generate bigrams, remove stop words, and calculate the frequency of bigrams
bigrams <- listings_data %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

# Visualization 7: Visualize the bigrams using a word cloud
set.seed(1234)  # for reproducibility
wordcloud(words = bigrams$bigram, freq = bigrams$n, min.freq = 1,
                  max.words = 100, random.order = FALSE, rot.per = 0.35, 
                  colors = brewer.pal(8, "Dark2"))

#### RShiny DashBoard ############

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Data Insights"),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Filters", tabName = "filters", icon = icon("filter"),
               checkboxGroupInput("sentimentScoreInput", "Choose Sentiment Scores:",
                                  choices = c("AFINN Score" = "afinn_score", 
                                              "NRC Score" = "nrc_score",
                                              "Bing Score" = "bing_score")),
               selectInput("roomTypeInput", "Room Type:", 
                           choices = c("All", unique(listings_data$room_type))),
               selectInput("cancellationPolicyInput", "Cancellation Policy:", 
                           choices = c("All", unique(listings_data$cancellation_policy))),
               sliderInput("reviewSlider", "Number of Reviews Range:",
                           min = 0, max = 140, 
                           value = c(min(listings_data$number_of_reviews, na.rm = TRUE), 
                                     max(listings_data$number_of_reviews, na.rm = TRUE)))
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Main dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 6, solidHeader = TRUE, status = "primary", 
                    title = "Correlogram of Sentiment Scores and Log(Price)", 
                    plotOutput("plotCorrelogram", height = "400px")),
                box(width = 6, solidHeader = TRUE, status = "warning", 
                    title = "Scatter Plot of Number of Reviews and Price", 
                    plotOutput("plotScatterReviewsPrice", height = "400px"))
              ),
              fluidRow(
                box(width = 6, solidHeader = TRUE, status = "info", 
                    title = "Price Distribution by Room and Top 5 Property Types", 
                    plotOutput("plotPriceRoomType", height = "400px")),
                box(width = 6, solidHeader = TRUE, status = "success", 
                    title = "Cancellation Policy and Price Distribution", 
                    plotOutput("plotCancellationPolicyPrice", height = "400px"))
              )
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Reactive expression to filter listings_data based on user input
  filtered_data <- reactive({
    req(listings_data) # Ensure listings_data is available
    data <- listings_data
    
    if(input$roomTypeInput != "All") {
      data <- data %>% filter(room_type == input$roomTypeInput)
    }
    
    if(input$cancellationPolicyInput != "All") {
      data <- data %>% filter(cancellation_policy == input$cancellationPolicyInput)
    }
    data <- data %>% filter(number_of_reviews >= input$reviewSlider[1],
                            number_of_reviews <= input$reviewSlider[2])
    data
  })
  
  # Render the Correlogram Plot dynamically based on selected sentiment score
  output$plotCorrelogram <- renderPlot({
    req(input$sentimentScoreInput)  # ensure that the input is not NULL
    scores_to_use <- c(input$sentimentScoreInput, "log_price")
    
    # Check if any scores were selected and if not default to a meaningful subset, e.g., all scores
    if (length(scores_to_use) == 1) {
      scores_to_use <- c("afinn_score", "nrc_score", "bing_score", "log_price")
    }
    
    ggpairs(sentiment_data[scores_to_use], 
            title = "Correlogram of Sentiment Scores and Log(Price)") +
      theme_minimal()
  })
  
  # Scatter Plot of Number of Reviews and Price (Log Scale)
  output$plotScatterReviewsPrice <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = number_of_reviews, y = price)) +
      geom_point(alpha = 0.5, color = "darkorange") +
      geom_smooth(method = "lm", se = FALSE, color = "dodgerblue") +
      scale_y_log10(labels = scales::dollar) +
      scale_x_continuous(limits = c(0, max(data$number_of_reviews, na.rm = TRUE)), breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Scatter Plot of Number of Reviews and Price (Log Scale)", x = "Number of Reviews", y = "Price (Log Scale)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Price Distribution by Room and Top 5 Property Types
  output$plotPriceRoomType <- renderPlot({
    data <- filtered_data()
    top5_property_types <- names(sort(table(data$property_type), decreasing = TRUE))[1:5]
    data <- data %>% filter(property_type %in% top5_property_types)
    
    ggplot(data, aes(x = room_type, y = price, fill = property_type)) +
      geom_boxplot(outlier.shape = NA) +
      coord_cartesian(ylim = c(0, quantile(data$price, 0.95, na.rm = TRUE))) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Price Distribution by Room and Top 5 Property Types", x = "Room Type", y = "Price (Limited to 95th percentile)") +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Cancellation Policy and Price Distribution (Log Scale)
  output$plotCancellationPolicyPrice <- renderPlot({
    data <- filtered_data()
    
    ggplot(data, aes(x = cancellation_policy, y = price, fill = cancellation_policy)) +
      geom_violin(trim = FALSE) +
      scale_y_log10(labels = scales::comma) +
      labs(title = "Cancellation Policy and Price Distribution (Log Scale)", x = "Cancellation Policy", y = "Price (Log Scale)") +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# Run the Shiny application
shinyApp(ui, server)

