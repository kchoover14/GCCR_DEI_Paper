library(rworldmap); library(countrycode); library(viridis)

#read data
dei.map = read.csv("data-dei-survey2022-v2-clean.CSV", stringsAsFactors = TRUE)

#country table
table(dei.map$Country) #n=102
#USA=24, Turkey=10, Italy=10, France=8, UK=5

#iso variable with iso3 names
dei.map$iso <- countrycode(dei.map$Country, origin = 'country.name', destination = 'iso3c')

#create dataframe for map
mapdf <- as.data.frame(table(dei.map$iso))
mapdf <- rename(mapdf, Country = Var1)
freq.sum = sum(mapdf$Freq)
mapdf$freq.propor <- ((mapdf$Freq / freq.sum) * 100)
mapdf.map = joinCountryData2Map(mapdf, joinCode = "ISO3",nameJoinColumn = "Country")
colors3 <- viridis(7, alpha = 1, begin = .1, end = .7, direction = -1, option = "D")

#join dataframe to the country map data
png(filename = "fig-response map.png", width=8, height= 4,
    units = "in",res=300, pointsize=10, type="cairo")
mapCountryData(mapdf.map, nameColumnToPlot="freq.propor", catMethod = "fixedWidth",
               missingCountryCol = grey(.99), colourPalette = colors3,
               mapTitle = "", addLegend = TRUE, oceanCol = "slategray1")
dev.off()

#read data for GCCR members and create country frequency list
member.map = read.csv("data-gccr membership-21oct21.csv")
member.map$country <- tolower(member.map$Country.of.Institution)
member.map$country[member.map$country == "united states of america"] <- "usa"
member.map$country[member.map$country == "united states"] <- "usa"
member.map$country[member.map$country == "congo, democratic republic of the"] <- "congo"
member.map$country[member.map$country == "cote d'ivoire"] <- "côte d'ivoire "
member.map$country[member.map$country == "korea, south"] <- "korea, republic of"
member <- select(member.map, country)
table(member)

#iso variable with iso3 names
member.map$iso <- countrycode(member.map$Country, origin = 'country.name', destination = 'iso3c')

#create dataframe for map
mapdf2 <- as.data.frame(table(member.map$iso))
mapdf2 <- rename(mapdf2, Country = Var1)
freq.sum = sum(mapdf2$Freq)
mapdf2$freq.propor <- ((mapdf2$Freq / freq.sum) * 100)
mapdf2.map = joinCountryData2Map(mapdf2, joinCode = "ISO3",nameJoinColumn = "Country")
colors3 <- viridis(7, alpha = 1, begin = .1, end = .7, direction = -1, option = "D")

#join dataframe to the country map data
png(filename = "fig-member map.png", width=8, height= 4,
    units = "in",res=300, pointsize=10, type="cairo")
mapCountryData(mapdf2.map, nameColumnToPlot="freq.propor", catMethod = "fixedWidth",
               missingCountryCol = grey(.99), colourPalette = colors3,
               mapTitle = "", addLegend = TRUE, oceanCol = "slategray1")
dev.off()
rm(list = ls())
