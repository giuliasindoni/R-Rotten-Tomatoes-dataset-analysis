data_review <- read.csv("rotten_tomatoes_critic_reviews.csv", header = TRUE)

data_movie <- read.csv("rotten_tomatoes_movies.csv", header = TRUE)




colnames(data_movie)

data_movie$directors

#--------- Exploring data

nrow(data_movie)
ncol(data_movie)
head(data_movie)
tail(data_movie)
str(data_movie)
summary(data_movie)
colnames(data_movie)


#------ Subsetting 
data_movie$directors
data_movie[,"directors"]
data_movie[1:10,1:10]
data_movie[c(4, 100),"movie_title"]
is.data.frame(data_movie)


#------------ FILTERING 

#---define as "Lynch" the subset of the dataframe where the director name is Lynch
#--- we get all the data about Lynch's movies

Lynch <- data_movie[data_movie$directors == "David Lynch",]
Lynch

is.data.frame(Lynch)
typeof(Lynch)

#------------ Look for movies (titles) in the dataset by David Lynch
#------------ any of these commands work

data_movie[data_movie$directors == "David Lynch","movie_title"]
data_movie[data_movie$directors == "David Lynch",2]
Lynch[,"movie_title"]
Lynch[,2]

#----- Titles by Lynch with Rotten status (i.e. less than 60% of fresh or cert. fresh critics' review )

Rotten_Lynch <- data_movie[data_movie$directors == "David Lynch" & data_movie$tomatometer_status == "Rotten","movie_title"]

Rotten_Lynch

#---- shorter way to define Rotten_Lynch is the following, using the Lynch dataframe defined earlier

Lynch[Lynch$tomatometer_status == "Rotten","movie_title"]

#----- Titles by Lynch with Tomatometer less than 50 (notice there are none)


LessThan_50percent_Lynch <- data_movie[data_movie$directors == "David Lynch" & data_movie$tomatometer_rating < 50,"movie_title"]

#----- Titles by Lynch with Fresh status(i.e. between 60 and 90% of fresh reviews)
# their tomatometer and their audience score

Fresh_Lynch <- Lynch[Lynch$tomatometer_status == "Fresh", "movie_title"]
Fresh_Lynch

#----- Titles by Lynch with Fresh and Certified fresh status (i.e. mor ethan 60% fresh reviews)
# their tomatometer and their audience score

AllFresh_Lynch <- data_movie[data_movie$directors == "David Lynch" & (data_movie$tomatometer_status == ("Fresh") | data_movie$tomatometer_status == ("Certified-Fresh")) , c(2, 15, 18) ]

AllFresh_Lynch

#------- All data on movies by A. Hitchcock
Hitchcock <- data_movie[data_movie$directors == "Alfred Hitchcock",]

Hitchcock

#----- Titles by Hitchcok with Rotten status 
Rotten_Hitchcock <- Hitchcock[Hitchcock$tomatometer_status == "Rotten","movie_title"]

#----- Titles by Hitchcok with Rotten status and their tomatometer scores (i.e. keep columns movie_title = 2 and tomatometer_status = 15 15)

Rotten_Hitchcock_withrating <- Hitchcock[Hitchcock$tomatometer_status == "Rotten", c(2, 15)]

Rotten_Hitchcock_withrating


is.data.frame(Hitchcock)
 
#---- Fresh and Cert. fresh titles by Hitchcock with tomatometer and audience score

AllFresh_Hitchcock <- Hitchcock[Hitchcock$tomatometer_status == ("Fresh") | Hitchcock$tomatometer_status == ("Certified-Fresh") , c(2, 15, 18) ]

AllFresh_Hitchcock



#------------- Colelcting tomatometer ratings for Lynch

Lynch$tomatometer_rating

#------------- Colelcting audience ratings for Lynch

Lynch$audience_rating

#----- difference between tomatometer and audience rating 
#------ for Lynch is negative for 3 movies out of 8
#------- audience appreciate this director more than critics
Lynch$tomatometer_rating - Lynch$audience_rating


#----- difference between tomatometer and audience rating 
#------ for Hitchcock is positive for all movies except one
#------- critics appreciate this director more than audience

Hitchcock$tomatometer_rating - Hitchcock$audience_rating

#--------- movie titles by Lynch

Lynch_Titles <- Lynch[,"movie_title"]
Lynch_Titles

#------ this is a vector
Lynch_Titles[1]


#----- titles and release dates
Lynch[,c(2, 10, 15)]

#----- movies are not ordered in time of release date.We need to sort them

Lynch[,c(10)]
min(Lynch[, c(10)])

sort(Lynch[,c(10)], decreasing = FALSE )

Lynch_release_dates_sorted <- sort(Lynch[,c(10)], decreasing = FALSE )

Lynch_release_dates_sorted

#---- titles of first and last movie directed by Lynch

First_Lynch_movie <- Lynch[Lynch$original_release_date == min(Lynch[, c(10)]), "movie_title"]


Last_Lynch_movie <- Lynch[Lynch$original_release_date == max(Lynch[, c(10)]), "movie_title"]
 

Last_Lynch_movie


#-------- PLOTTING

#----- Plot how critics rating of Lynch's movies evolved through time

qplot(data = Lynch, x = original_release_date, y = tomatometer_rating)

colnames(Lynch)


#----- Plot how critics rating of Hitchcok's movies evolved through time

Hitchcock[, "tomatometer_rating"]

Hitchcock[, "original_release_date"]


qplot(data = Hitchcock, x = original_release_date, y = tomatometer_rating)

#---- create new dataframe for W. Allen movies info by subsetting main dataframe
#--- then plot it
Allen <- data_movie[data_movie$directors == "Woody Allen", ]

colnames(Allen)

qplot(data = Allen, x = original_release_date, y = tomatometer_rating)


#------ Add new column: year of release 

Lynch$year_of_release <- c(1984, 1980, 1986, 1977, 1997, 2001, 1999, 1992)
Lynch[,c(2, 23)]


#------- Plot critics rating against time using year of release

qplot(data = Lynch, x = year_of_release, y = tomatometer_rating)


#----- Extracting year of release from date of release vector
#----- and adding it as a new column of data_movie 

#-- first make sure original release date column is is date format
data_movie$original_release_date <- as.Date(data_movie$original_release_date)

#-- then add the new column of the year taking the year from the original release date column
data_movie$year_of_release <- as.numeric(format(data_movie$original_release_date, "%Y"))

colnames(data_movie)

#--- check that Hitchcok, for example as the new column now (need to rerun definition of data_movie and
#--- Hitchock from above)

head(Hitchcock)

#---- Now we plot Hitchcock rating against the years

qplot(data = Hitchcock, x = year_of_release, y = tomatometer_rating)

#---- Now also original dates is shown as years in y axis
#----- The date is placed across the years span 

qplot(data = Hitchcock, x = original_release_date, y = tomatometer_rating)


Hitchcock[Hitchcock$year_of_release == 1938, c(2, 15)]

Hitchcock[Hitchcock$year_of_release=="1938", c(2, 10, 15, 23)]

#---- Let's add some colors to categorize movies by genre, and some more details 

qplot(data = Hitchcock, x = original_release_date, y = tomatometer_rating, colour= genres,
      size = I(3), alpha = I(0.7), main = "Hitchcock movies tomatometer score during the years")

#----Also here, if we plot original date of release
#----the tick marks show the years. However there is higher accuracy than 
#----year of release plot. Indeed for example the movie Dune was released on December 1984
#----the plot with  original date of release shows it almost on the year 1985, as the date is
#----closer to 1985 than to 1984
#----Whilst the plot with year of release shows it on 1984, as that the year that can be fecthed 
#----from the date

qplot(data = Lynch, x = year_of_release, y = tomatometer_rating)

qplot(data = Lynch, x = original_release_date, y = tomatometer_rating)

Lynch[, c(2, 10, 15)]

#----- Let's add some color and more details 

qplot(data = Lynch, x = original_release_date, y = tomatometer_rating, colour = genres,
      size= I(3), alpha = I(0.6), 
      main ="Lynch movies tomatometer score over the years")

#---- Same plots with Allen data 
qplot(data = Allen, x = year_of_release, y = tomatometer_rating)
qplot(data = Allen, x = original_release_date, y = tomatometer_rating)


#----- Let's add some color
qplot(data = Allen, x = original_release_date, y = tomatometer_rating, colour = genres,
      size = I(5), alpha = I(0.5), main = "Allen's movies tomatometer score the years")


#---- Plotting using ggplot instead of qplot (i.e. quick plot)

Allen_plot <- ggplot(data = Allen, aes(x = original_release_date,
                                       y = tomatometer_rating,  colour = genres,
                                        alpha = I(0.5)) )

#--- Adding a smoother to the plot so we can see a trend, 
#--- for each genre

Allen_plot + geom_point() + geom_smooth(fill = NA)


#----- Lynch plot using ggplot
#--- here if we divide by genre we are not able to see any trend 
#--- there are not enough movies

Lynch_plot <- ggplot(data = Lynch, aes(x = original_release_date,
                                       y = tomatometer_rating,  colour = genres,
                                       alpha = I(0.8)) )

Lynch_plot + geom_point() + geom_smooth(fill = NA)


#--- So let's remove the coloring by genre
#so we can observe a trend

Lynch_plot2 <- ggplot(data = Lynch, aes(x = original_release_date,
                                       y = tomatometer_rating,
                                       alpha = I(0.8)) )

Lynch_plot2 + geom_point() + geom_smooth(fill = NA)

#---- Same work for Hitchcock

Hitchcock_plot <- ggplot(data = Hitchcock, aes(x = original_release_date,
                                       y = tomatometer_rating,
                                       alpha = I(0.8)) )

Hitchcock_plot + geom_point() + geom_smooth(fill=NA)

#----- Let's try stat_smooth: same results

Allen_plot + geom_point() + stat_smooth(fill = NA)

Hitchcock_plot + geom_point() + stat_smooth(fill = NA)

Lynch_plot2 + geom_point() + stat_smooth(fill = NA)

#------ How many movies of Hitchcock?
count_Hitchcock <- 0
for (director in data_movie$directors) {
  if(director == "Alfred Hitchcock"){
    count_Hitchcock <- count_Hitchcock +1
    
  }
  
}
print(count_Hitchcock)

nrow(Hitchcock)

#---- extend to all directors 

directors <- data_movie$directors


#---- Now we plot how these directors have been rated 


Lynch_audience_plot <- ggplot(data = Lynch, aes(x = original_release_date,
                                                y = audience_rating,  colour = genres,
                                                alpha = I(0.8)) )
Lynch_audience_plot + geom_point()

Lynch_audience_plot2 <- ggplot(data = Lynch, aes(x = original_release_date,
                                                 y = audience_rating,
                                                alpha = I(0.8)) )
#---- Let's also add the tile with ggplot, and some new names for the axis

Lynch_audience_plot2 + geom_point() + geom_smooth(fill = NA) +
  ggtitle("Audience rating of Lynch's movies through the years") +
  labs(y= "Audience rating score", x = "Years" )

#--------- Same withe Allen

Allen_audience_plot <- ggplot(data = Allen, aes(x = original_release_date, 
                                                y = audience_rating,
                                                color = genres,
                                                alpha = I(0.8)
                                                
                                                ))

Allen_audience_plot + geom_point() + geom_smooth(fill = NA) +
  ggtitle("Audience rating of Allen's movies through the years \ncategorised by genre") +
  labs( y= "Audience rating", x = "Years" )

#---- let's try Allens plot without color categorisation

Allen_audience_plot2 <- ggplot(data = Allen, aes(x = original_release_date, 
                                                 y = audience_rating,
                                                 
                                                 ))
#---- We can see better tha, overall, Allen's rating by audience has gone down
Allen_audience_plot2 + geom_point() + geom_smooth(fill = NA) + ggtitle("Audience rating of Allen's movies through the years") +
  labs( y= "Audience rating", x = "Years" )

#-------- Let's plot the same infor fo Hitchcock

Hitchcock_audience_plot <- ggplot(data = Hitchcock, aes(x = original_release_date, 
                                                    y = audience_rating,
                                                    
))

#---- We can see that there is more variace in the way audience rated Hitchcock
#---- movies, although the trend is similar.

Hitchcock_audience_plot + geom_point() + geom_smooth(fill = NA) + ggtitle("Audience rating of Hitchcock's movies through the years") +
  labs( y= "Audience rating", x = "Years" )

Hitchcock_plot + geom_point() + geom_smooth(fill = NA)

Hitchcock_audience_plot + geom_point() + geom_smooth(fill = NA)


#---- Compare audience and tomatometer for a director ex: Hitchcock





#---- Let's work with more date: we could see how a certain genre's rating (example comedy)
#---- evolved through time?
#---- Or how audience and tomatometer ratings relate

#--- First let's group together all movies info 
#--- that are also comedies
#--- this is hard because there are lots of intersections of genres
#--- so we just consider the following intersections

comedy <- data_movie[(data_movie$genres == "Comedy" | data_movie$genres == "Comedy, Romance" |
                        data_movie$genres == "Comedy, Drama, Romance" |
                        data_movie$genres == "Classics, Comedy, Romance" |
                        data_movie$genres == "Comedy, Horror, Mystery & Suspense" |
                        data_movie$genres == "Comedy, Drama" |
                        data_movie$genres == "Action & Adventure, Comedy"|
                        data_movie$genres == "Comedy, Drama, Mystery & Suspense" |
                        data_movie$genres == "Classics, Comedy, Mystery & Suspense" |
                        data_movie$genres == "Classics, Comedy, Science Fiction & Fantasy"|
                        data_movie$genres == "Classics, Comedy, Drama") , ]

is.data.frame(comedy)


data_movie$genres

#--- We collected 3101 comedies 
#--- is there a way to fetch the word "Comedy" from description?
#--- and collect all those movie?

#---- Some checks

nrow(comedy)

tail(comedy$directors)

comedy[comedy$directors == "Woody Allen", ]

#-- Now we plot tomatometer (i.e. critics) rating 
#-- against audience rating for comedies

colnames(comedy)

comedy_critics_audience_plot <- ggplot(data = comedy, aes(x = tomatometer_rating, y = audience_rating,
                                                           alpha(I(0.5))))
#--- positive correlation between critics rating and audience rating
#--- in comedy movies
comedy$tomatometer_rating

comedy_critics_audience_plot + geom_point() + stat_smooth()


#---- Let's plot evolution of comedy's rating through time

#--- first for critics rating 
comedy_critics_time_plot <- ggplot(data = comedy, aes(x = original_release_date, y = tomatometer_rating,
                                                          alpha(I(0.5))))

comedy_critics_time_plot + geom_point() + geom_smooth(fill = NA) + 
  ggtitle("Evolution of comedy rating by critics through time") + 
  labs(x = "Year", y = "Tomatometer")

#-- then for audience rating 

comedy_audience_time_plot <- ggplot(data = comedy, aes(x = original_release_date, y = audience_rating,
                                                      alpha(I(0.5))))

comedy_audience_time_plot + geom_point() + geom_smooth(fill = NA)  + 
  ggtitle("Evolution of comedy rating by audience through time") + 
  labs(x = "Years", y = "Audience score")


#---- How many movies for each genre?
#---- How many comedy movies for each year/interval of years?
#---- Histograms? 

myhistogram_comedy <- ggplot(data = comedy, aes(x = year_of_release)) 

myhistogram_comedy +  geom_histogram(binwidth = 10)

colnames(data_movie)

myhistogram_genres <- ggplot(data = data_movie, aes(x = genres))

myhistogram_genres + geom_bar()




