# Groupwork 2: Text Mining

We are the communication department of Berner Fachhochschuleand manage various channels of which Twitter is one. Next to our own activities on this social media platform, we are interested in finding out how other Universities of Applied Sciencesuse Twitter. Any insights are of value, because such can help us to further improve our communication strategy on Twitter and beyond.

1. How many tweets are being posted by the various Universities when? Are there any “release“ strategies visible?
2. What are the tweets about and how do other Twitter users react to them (likes, etc.)?
3. How do the university tweets differ in terms of content, style, emotions, etc?
4. What specific advice can you give us as communication department of BFH based on your analysis? How can we integrate the analysis of tweets in our internal processes, can you think of any data products that would be of value for us?
   70

Dataset: Tweets created by one of the swiss universities of applied sciences (until Jan/23)
Quantity: 19.575 tweets
File: Tweets_all.rda
Variables: ID & Id_str(of the tweet as integer and as string), full_text, in_reply_to_screen_name, retweet_count(how often a tweet was re-posted by Jan/23), favorite_count(how often a tweet was liked by Jan/23), lang (language of the tweet), university (which univ. of applied science created the tweet), Various forms the tweet‘s timestamp (e.g. created_at, tweet_date, tweet_month, tweet_minute).
Further hints: How to deal with re-tweets, automated tweets & chatbots and emojis?

## Data Structure

Unnamed: 0: An index or identifier for each tweet. It's an integer value.
created_at: The timestamp when the tweet was created, in the format "YYYY-MM-DD HH:MM:SS".
id: The numeric ID of the tweet, in floating-point format.
id_str: The string representation of the tweet's ID.
full_text: The full text of the tweet.
in_reply_to_screen_name: The screen name of the user to whom the tweet was a reply, if any. Most of these values are missing (NaN), indicating that most tweets are not replies.
retweet_count: The number of times the tweet has been retweeted.
favorite_count: The number of times the tweet has been liked (favorited).
lang: The language code of the tweet, e.g., "de" for German.
university: Appears to be an identifier or name of the university associated with the tweet.
tweet_date: The date the tweet was posted, in "YYYY-MM-DD" format.
tweet_minute: The timestamp rounded to the nearest minute, in "YYYY-MM-DD HH:MM" format.
tweet_hour: The timestamp rounded to the nearest hour, in "YYYY-MM-DD HH:00" format.
tweet_month: The first day of the month in which the tweet was posted, in "YYYY-MM-DD" format, where the day is always "01".
timeofday_hour: The hour of the day (0-23) when the tweet was posted.

## Group Plan:

Step 1: Project Initialization & Team Alignment

1. Initial Meeting and Role Distribution (1 hour)
   • Conduct a kickoff meeting to understand project goals and allocate
   tasks among team members.
   • Ensure clarity on the assignment requirements and timeline.
   Step 2: Context Understanding & Dataset Familiarization
1. Contextual Research (1 hour)
   • Briefly research how universities use Twitter in their communication
   strategies.
   • Gather insights on the significance of social media for university
   outreach.
1. Dataset Loading and Preliminary Exploration (2 hours)
   • Load the "Tweets_all.rda" dataset in RStudio.
   • Conduct an initial exploration to understand the dataset's structure,
   including variables and their types (numeric, text, categorical).
   Step 3: Data Preprocessing
1. Text Data Cleaning (2 hours)
   • Clean the tweet texts, handling emojis, URLs, and mentions.
   • Normalize text data by tokenization and removing stop words.
1. Handling Missing Values and Duplicates (1 hour)
   • Identify and address missing values and duplicate records.
   Step 4: Exploratory Data Analysis (EDA)
1. Tweet Frequency and Engagement Analysis (2 hours)
   • Analyze the frequency of tweets over time and their engagement
   metrics (likes, retweets).
   • Identify any visible patterns or strategies used by universities.
1. Content Analysis (3 hours)
   • Perform basic text mining to categorize tweets and uncover common
   themes.
   • Use sentiment analysis to gauge the emotional tone of the tweets.
   Step 5: Comparative Analysis & Insight Generation
1. Comparative Content and Style Analysis (3 hours)
   • Compare tweets between universities to identify differences in content,
   style, and engagement.
   • Utilize visualizations to highlight findings.
1. Insight Synthesis and Recommendations (2 hours)
   • Synthesize insights from analyses to draft actionable recommendations
   for improving communication strategies.
   Step 6: Presentation Preparation
1. Development of Presentation Materials (2 hours)
   • Create a presentation that succinctly summarizes the project's context,
   findings, analyses, and recommendations.
   • Design clear and informative visualizations to support the narrative.
1. Presentation Review and Dry Run (1 hour)
   • Conduct a comprehensive review of the presentation content and flow.
   • Perform a dry run to ensure smooth delivery and timing.
   Total Estimated Time: 19-20 hours
   Important:
   • Coaching: 23th of April & 14th of May!
