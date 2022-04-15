library(tidyverse)
library(ggplot2); library(ggpubr); library(patchwork);
library(png); library(geomtextpath)
library(viridis); library(hrbrthemes)
library(bibliometrix)

#links only visible with WoS subscription
#https://www.webofscience.com/wos/woscc/summary/6806fa57-3edb-408d-802b-54458855247b-25226194/relevance/1
#Data
db.statementORplan <- convert2df('data-wos-statement or plan-1mar22.txt', db = "wos", format = "plaintext")

#summary
results.statementORplan <- biblioAnalysis(db.statementORplan, sep = ";")
summary.statementORplan <- summary(object = results.statementORplan, k = 15, pause = FALSE)
maininfo.statementORplan <- summary.statementORplan$MainInformationDF
write.csv(maininfo.statementORplan, "result-maininfo.statementORplan.csv", quote = FALSE, row.names = FALSE)

#annual productivity
annpro.statementORplan <- summary.statementORplan["AnnualProduction"]
annpro.statementORplan <- bind_rows(annpro.statementORplan)
write.csv(annpro.statementORplan, "result-annual productivity.statementORplan.csv", quote=FALSE, row.names = FALSE)
rm(annpro.statementORplan)
annpro.statementORplan <- read.csv("result-annual productivity.statementORplan.csv")
annpro.statementORplan <- annpro.statementORplan %>% filter(Year < 2022)

p1a <- ggplot(annpro.statementORplan, aes(x = Year, y = Articles), fill=year) +
  geom_area(size=1.5, fill="#440154FF") +
  xlab("Year of Publication") + ylab("Number of Articles") +
  scale_x_continuous(breaks = seq(1994, 2021, 5)) +
  geom_point(size=1.5, color='black') +
  theme_pubr()
p1 <- p1a + theme(
  axis.title.x = element_text(color = "black", size = 14, face = "bold"),
  axis.title.y = element_text(color = "black", size = 14, face = "bold"))

#productivity by country
prod.country.statementORplan <- summary.statementORplan["MostProdCountries"]
prod.country.statementORplan <- bind_rows(prod.country.statementORplan)
write.csv(prod.country.statementORplan, "result-country productivity.statementORplan.csv", quote=FALSE, row.names = FALSE)
rm(prod.country.statementORplan)
prod.country.statementORplan <- read.csv("result-country productivity.statementORplan.csv")
prod.country.statementORplan <- prod.country.statementORplan %>% select(!c(Freq, MCP_Ratio))
prod.country.statementORplan <- prod.country.statementORplan %>% gather(key = "Type", value = "Number", SCP, MCP)
prod.country.statementORplan$Type <- factor(prod.country.statementORplan$Type)

prod.country.statementORplan$Country <- fct_rev(fct_reorder(prod.country.statementORplan$Country, desc(prod.country.statementORplan$Articles)))
p2a <- ggplot(prod.country.statementORplan, aes(Number, Country, fill=Type)) +
  geom_bar(stat="identity") +
  xlab("Number of Articles for Top Countries") + ylab("Top 15 Countries") +
  scale_fill_viridis_d(direction = -1, begin = .1, end = .6,
                       name = "", labels = c("Multi-Country", "Single Country")) +
  theme_pubr() +
  theme(legend.position = c(0.88, 0.15),legend.background =
          element_rect(fill = "white", color = "#440154"))
p2 <-p2a + theme(axis.text.y = element_text(hjust = 0)) +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"))

#Citation by country
cit.country.statementORplan <- summary.statementORplan["TCperCountries"]
cit.country.statementORplan <- bind_rows(cit.country.statementORplan)
write.csv(cit.country.statementORplan, "result-country productivity.statementORplan.csv", quote=FALSE, row.names = FALSE)
rm(cit.country.statementORplan)
cit.country.statementORplan <- read.csv("result-country productivity.statementORplan.csv")

cit.country.statementORplan$Country <- fct_rev(fct_reorder(cit.country.statementORplan$Country, desc(cit.country.statementORplan$Total.Citations)))
p3a <- ggplot(cit.country.statementORplan, aes(Total.Citations, Country)) +
  geom_bar(stat="identity", fill = "#440154") +
  xlab("Total Citations per Country") + ylab("Top 15 Countries") +
  theme_pubr() +
  theme(legend.position = "none")
p3 <-p3a + theme(axis.text.y = element_text(hjust = 0)) +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"))


#add collaboration map
collab.map <- png::readPNG("F1C-CountryCollaborationMap.png", native = TRUE)

#Panel Plot
png("F1-bibmet-general trends.png", family="sans", res = 300,
    height = 10, width = 20, units = "in")
fig1 <- p1 + p2 + collab.map + p3 +
  plot_layout(widths = c(1.5, 1))
fig1 + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 30))
dev.off()

