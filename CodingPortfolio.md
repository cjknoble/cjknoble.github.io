[RETURN HOME](https://cjknoble.github.io/)

# Coding Portfolio

### High Level Covariation Mining
![Image](./assets/img/Rplot01.png)
- **Description:** This code was utilized to generate relationships within a system dynamics model. The script performs data analysis by fitting multivariate linear regression and grey models with various transformations. It includes functions for generating grey model parameters, plotting scatter plots with different transformations, calculating and recording R-squared values from linear models, and generating and storing regression outputs, all while handling data preprocessing and visualization.
- **Language:** R
- **Link:** [R Script Text File](./assets/codefiles/R - High Level Covariation Mining.txt)

<br>
### Mining Google Search and Google Trends Data
![Image](./assets/img/googlescrape.png)
- **Description:** This code performs web scraping to collect Google search result counts and Google Trends data for specified keywords over a selected timeframe. Using the RSelenium package, it navigates to Google search pages for each keyword and extracts the total number of search results for each month. Additionally, it fetches historical Google Trends data for the same keywords using the gtrendsR package, compiling the data into dataframes that are then exported as CSV files for further analysis.
- **Language:** R
- **Link:** [R Script Text File](./assets/codefiles/R - Mining Google Search and Trends.txt)

<br>
### Collecting, Cleaning, and Analyzing Tweets 
![Image](./assets/img/twitterclean.png)
- **Description:** This code retrieves and processes geolocated tweets in New Jersey using the Twitter API. It first sets up API tokens and constructs a query to fetch relevant tweets, which are then saved as JSON files. The script subsequently imports these JSON files, cleans and processes the tweet text by removing unnecessary characters and stopwords, and performs text analysis to tokenize and count word occurrences. Finally, it filters and summarizes the word counts, exporting the cleaned and analyzed data to a CSV file.
- **Language:** R
- **Link:** [R Script Text File](./assets/codefiles/R - Twitter Scraping .txt)

<br>
### Automating Spatial Joins in GIS using ArcPy
![Image](./assets/img/arcpy.png)
- **Description:** This Python script uses the ArcPy library to process point data by importing tables, adding spatial attributes, and performing spatial joins to assign zone information. It then exports the processed data and converts them to text files for further analysis.
- **Language:** Python
- **Link:** [Python Script Text File](./assets/codefiles/Python - Spatial Join Automation with ArcPy.txt)
