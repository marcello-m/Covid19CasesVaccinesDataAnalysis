# import the datasets
cases_full <- read.csv("csv/cases.csv")
vaccine_full <- read.csv("csv/vaccines.csv")

# import libraries
library(pracma)
library(dplyr)
library(ggplot2)
library(gridExtra)
# library(ggmap) abilitate this library if you plan to use a Google API key to generate maps
library(ggrepel)
library(RColorBrewer)

# image save path
IMAGE_PATH = "./plots/"

###########################################################################################

### DATA CLEANING ###

###########################################################################################

## CASES DATASET ##

cases_c <- subset(cases_full, select=-c(continentExp)) # remove 'Europe' column
# create a weekly dataset for the future comparison with the vaccines dataset
cases_weekly <- cases_c 
ndays <- vector("numeric", length = nrow(cases_c))
trentag <- c(4,6,9,11)
month_days <- vector("numeric", length = 11)

for(i in 1:11){
	sum <- 0
	for(j in 1:i){
    	if(j==2){
		    sum <- sum + 28
    	} else if(j%in%c(4,6,9,11)){
    	  sum <- sum + 30
    	} else {
    	  sum <- sum + 31    
    	}
	}	
	month_days[i] <- sum
}

for(i in 1:nrow(cases_weekly)) {
	day <- cases_weekly$day[i]
    month <- cases_weekly$month[i]
	ndays[i] <- day+month_days[month-1]
}

nweek <- vector("numeric", length = length(ndays))
for(i in 1:length(ndays)){
	nweek[i] <- floor(((ndays[i]-4)/7)+1)
}
YearWeekISO <- vector(mode="character", length=length(nweek)) # ISO 8601
for(i in 1:length(nweek)){
	if(nweek[i]<10){
		YearWeekISO[i] <- paste0("2021-W0",nweek[i])
	}else{
		YearWeekISO[i] <- paste0("2021-W",nweek[i])
	}
}
cases_weekly <- cbind(cases_weekly, YearWeekISO)
# cases_weekly <- cases_weekly[,c(9,2,3,4,5,6,7,8)]
cases_weekly <- cases_weekly[,c(11,5,6,7,9,10)]
cases_weekly_tot <- as.data.frame(summarize(group_by(cases_weekly,countriesAndTerritories,YearWeekISO), unique(countryterritoryCode), sum(cases) ,sum(deaths), unique(popData2020)))
colnames(cases_weekly_tot)<-c("countriesAndTerritories","YearWeekISO","countryterritoryCode","cases","deaths","popData2020")

# convert the date from string to Date format, remove excess columns, reorder columns
dateRep <- as.Date(cases_c$dateRep, format="%d/%m/%Y")
cases_c <- subset(cases_c, select=-c(geoId, day, month, year, dateRep))
cases_c <- cbind(cases_c, dateRep)
cases_c <- cases_c[,c(6,1,2,3,4,5)]

###########################################################################################

## VACCINES DATASET ##

# deleting regional data
countries <- unique(vaccine_full$ReportingCountry)
vaccine_c <- filter(vaccine_full, Region %in% countries)

# removing column with high % of NA values
vaccine_c <- subset(vaccine_c, select=-c(FirstDoseRefused))

# dividing vaccines by mono and double doses
vaccine_mono <- filter(vaccine_c, Vaccine=="JANSS")
vaccine_double <- filter(vaccine_c, Vaccine!="JANSS")

# creating new age groups equal for all countries
# creating 4 main groups (<18, 18-49, 50-79, >=80)
f1 <- c("Age0_4","Age5_9","Age10_14","Age15_17")
f2 <- c("Age18_24","Age25_49")
f3 <- c("Age50_59","Age60_69","Age70_79")
# f4 not specified

vaccine_age <- as.data.frame(vaccine_c %>% group_by(ReportingCountry, TargetGroup)%>%summarise(FirstDose=sum(FirstDose),SecondDose=sum(SecondDose)))
FirstDose_age <- vector("numeric", length=4*length(countries))
SecondDose_age <- vector("numeric", length=4*length(countries))
k <- 1 # k index where to store data
j <- 1
for(i in 1:length(countries)){
	f1_flag <- 0
	repeat{
		# CONDITION FOR <18 GROUP
		if(strcmp(vaccine_age$TargetGroup[j],"Age<18")&vaccine_age$FirstDose[j]!=0&vaccine_age$SecondDose[j]!=0){
			FirstDose_age[k] <- FirstDose_age[k] + vaccine_age$FirstDose[j]
			SecondDose_age[k] <- SecondDose_age[k] + vaccine_age$SecondDose[j]
			f1_flag <- 1
		}
		else if(vaccine_age$TargetGroup[j]%in%f1 & f1_flag==0){
			FirstDose_age[k] <- FirstDose_age[k] + vaccine_age$FirstDose[j]
			SecondDose_age[k] <- SecondDose_age[k] + vaccine_age$SecondDose[j]
		}
		
		# CONDITION FOR 18-49 GROUP
		else if(vaccine_age$TargetGroup[j]%in%f2){
			FirstDose_age[k+1] <- FirstDose_age[k+1] + vaccine_age$FirstDose[j]
			SecondDose_age[k+1] <- SecondDose_age[k+1] + vaccine_age$SecondDose[j]
		}
		
		# CONDITION FOR 50-79 GROUP
		else if(vaccine_age$TargetGroup[j]%in%f3){
			FirstDose_age[k+2] <- FirstDose_age[k+2] + vaccine_age$FirstDose[j]
			SecondDose_age[k+2] <- SecondDose_age[k+2] + vaccine_age$SecondDose[j]
		}
		
		# CONDITION FOR 80+ GROUP
		else if(strcmp(vaccine_age$TargetGroup[j],"Age80+")){
			FirstDose_age[k+3] <- FirstDose_age[k+3] + vaccine_age$FirstDose[j]
			SecondDose_age[k+3] <- SecondDose_age[k+3] + vaccine_age$SecondDose[j]
		}
		j<-j+1
		if(j>nrow(vaccine_age)|!strcmp(vaccine_age$ReportingCountry[j],countries[i])){
			break
		}
	}
	k <- k+4
}
ReportingCountry_age <- rep(countries, each=4)
TargetGroup <- rep(c("Age<18","Age18_49","Age50_79","Age80+"),times=length(countries))
vaccine_age_c <- as.data.frame(ReportingCountry_age)
vaccine_age_c <- cbind(vaccine_age_c, TargetGroup, FirstDose_age, SecondDose_age)

# vaccine_age_c <- subset(vaccine_age_c,!(ReportingCountry_age%in%c("DE","LI","NL")))
# countries_age <- arrange(vaccine_tot, -(FirstDose+SecondDose))$ReportingCountry
# countries_age <- countries_age[-c(1,6,30)] # indexes of DE LI and NL (don't have age groups division)

###########################################################################################

### EXPLORATORY DATA ANALYSIS ###

###########################################################################################

## CASES DATASET ##

# creating dataset with summed up data (no time division)
cases_tot <- as.data.frame(summarize(group_by(cases_c,countriesAndTerritories), unique(countryterritoryCode), sum(cases) ,sum(deaths), unique(popData2020)))
names(cases_tot)[2]<-paste("countryterritoryCode")
names(cases_tot)[3]<-paste("cases")
names(cases_tot)[4]<-paste("deaths")
names(cases_tot)[5]<-paste("popData2020")

# deaths by eu country plot
ggplot(cases_tot,aes(reorder(countryterritoryCode,-deaths),deaths))+geom_bar(stat="identity",position="dodge",aes(fill=deaths)) + geom_text(aes(label=deaths), vjust=-0.9, position = position_dodge(0.9), size=3.5)+labs(x="Countries",y="Deaths")+ggtitle(label="Deaths by EU country")
ggsave("total_deaths_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# cases by eu country plot
ggplot(cases_tot,aes(reorder(countryterritoryCode,-cases),cases))+geom_bar(stat="identity",position="dodge",aes(fill=cases)) + geom_text(aes(label=cases), vjust=-0.9, position = position_dodge(0.9), size=3.5)+labs(x="Countries",y="Cases")+ggtitle(label="Cases by EU country")
ggsave("total_cases_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# deaths per thousand plot
deathsPerMille <- vector("numeric", length = nrow(cases_tot))
for(i in 1:nrow(cases_tot)){
	deathsPerMille[i] <- cases_tot$deaths[i]/(cases_tot$popData2020[i]/1000)
}
cases_tot <- cbind(cases_tot, deathsPerMille)

ggplot(cases_tot,aes(reorder(countryterritoryCode,-deathsPerMille),deathsPerMille))+geom_bar(stat="identity",position="dodge",aes(fill=deathsPerMille)) + geom_text(aes(label=sprintf("%0.2f", round(deathsPerMille, digits = 2))), vjust=-0.9, position = position_dodge(0.9), size=3.5)+labs(x="Countries",y="Deaths per 1000")+ggtitle(label="Deaths per thousand in EU countries")
ggsave("deaths_per_thousand_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# mortality rate plot
mortalityRate <- vector("numeric", length = nrow(cases_tot))
for(i in 1:nrow(cases_tot)){
	mortalityRate[i] <- (cases_tot$deaths[i]/cases_tot$cases[i])*100
}
cases_tot <- cbind(cases_tot, mortalityRate)

ggplot(cases_tot,aes(reorder(countryterritoryCode,-mortalityRate),mortalityRate))+geom_bar(stat="identity",position="dodge",aes(fill=mortalityRate)) + geom_text(aes(label=sprintf("%0.2f%%", round(mortalityRate, digits = 2))), vjust=-0.9, position = position_dodge(0.9), size=3.5)+labs(x="Countries",y="Mortality rate (%)")+ggtitle(label="Mortality rate in EU countries (%)")
ggsave("mortality_rate_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# cases per thousand plot
casesPerMille <- vector("numeric", length=nrow(cases_tot))
for(i in 1:nrow(cases_tot)){
	casesPerMille[i] <- cases_tot$cases[i]/(cases_tot$popData2020[i]/1000)
}
cases_tot <- cbind(cases_tot, casesPerMille)

ggplot(cases_tot,aes(reorder(countryterritoryCode,-casesPerMille),casesPerMille))+geom_bar(stat="identity",position="dodge",aes(fill=casesPerMille)) + geom_text(aes(label=sprintf("%0.2f", round(casesPerMille, digits = 2))), vjust=-0.9, position = position_dodge(0.9), size=3.5)+labs(x="Countries",y="Cases per 1000")+ggtitle(label="Cases per thousand in EU countries")
ggsave("cases_per_thousand_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# cases / deaths ratio plot
ggplot(cases_tot,aes(casesPerMille,deathsPerMille))+geom_point(stat="identity",position="dodge", aes(colour=countriesAndTerritories),size=3) + ggtitle(label="Cases to deaths")+geom_text_repel(aes(label=countryterritoryCode))+theme(legend.position = "none")
ggsave("cases_per_thousand_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# cases and deaths evolution in time
# create a column with cumulative sum of cases
countries_cases <- unique(cases_c$countriesAndTerritories)
cases_sum <- vector("numeric", length = nrow(cases_c))
j <- nrow(cases_c)
for(i in 1:length(countries_cases)){
	cases_sum[j] <- cases_c$cases[j]
	j <- j-1
	repeat{
		cases_sum[j] <- cases_sum[j+1] + cases_c$cases[j]
		j <- j-1
		if(!strcmp(cases_c$countriesAndTerritories[j],cases_c$countriesAndTerritories[j+1])){
			break
		}
	}	
}
cases_c <- cbind(cases_c, cases_sum)
cases_c <- cases_c[,c(1,2,7,3,4,5,6)]

# evolution of cases with all countries represented
colors <- c('#E666B3', '#33991A', '#CC9999', '#B3B31A', '#00E680', 
            '#4D8066', '#809980', '#E6FF80', '#1AFF33', '#999933',
            '#FF3380', '#CCCC00', '#66E64D', '#4D80CC', '#9900B3',
						'#FF99E6', '#CCFF1A', '#FF1A66', '#E6331A', '#33FFCC',
            '#66994D', '#B366CC', '#4D8000', '#B33300', '#CC80CC', 
            '#66664D', '#991AFF', '#E666FF', '#4DB3FF', '#1AB399')
p <- ggplot()
for(i in 1:length(countries_cases)){
	p <- p + geom_line(data=filter(cases_c, countriesAndTerritories==countries_cases[i]), aes(x=dateRep, y=cases_sum), color=colors[i], size=1.08)
}

ind <- vector("numeric", length=length(countries_cases))
j <- 0
for(i in 1:length(countries_cases)){
	while(strcmp(cases_c$countriesAndTerritories[j],cases_c$countriesAndTerritories[j+1])){
		j<-j+1
	}
	j<-j+1
	ind[i] <- j
}

p<-p+geom_text(aes(label=countries_cases, x=dateRep[ind], y=cases_sum[ind]), color=colors, hjust=-.1)
p<-p+scale_x_date(breaks = scales::breaks_pretty(10), limits=c(dateRep[length(dateRep)],as.Date("2021-11-15")))
p<-p+ggtitle(label="Evolution of total cases by EU country")+labs(x="Time",y="Total cases")
ggsave("cases_evolution_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# plot is too confused -> isolate only the top 5 countries
top_countries <- c("France","Germany","Italy","Poland","Spain")
top_colors <- c("Red","Blue","#0d6911","#cf6604","Black")
p2 <- ggplot()
for(i in 1:length(top_countries)){
	p2 <- p2 + geom_line(data=filter(cases_c, countriesAndTerritories==top_countries[i]), aes(x=dateRep, y=cases_sum), color=top_colors[i], size=1.08)
}

ind2 <- match(top_countries, countries_cases)
ind3 <- vector("numeric", length=length(ind2))
for(i in 1:length(ind2)){
	ind3[i]<-ind[ind2[i]]
}

p2<-p2+geom_text(aes(label=top_countries, x=dateRep[ind3], y=cases_sum[ind3]), color=top_colors, hjust=-.1)
p2<-p2+scale_x_date(breaks = scales::breaks_pretty(10), limits=c(dateRep[length(dateRep)],as.Date("2021-11-15")))
p2<-p2+ggtitle(label="Evolution of total cases by EU country in top 5 countries")+labs(x="Time",y="Total cases")
ggsave("cases_evolution_top_5_countries.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# now just the new daily cases for the top 5 countries
plt_cases <- list()
for(i in 1:5){
	data <- filter(cases_c, countriesAndTerritories==top_countries[i])
	plt_cases[[i]]<-ggplot()+geom_line(data[1:nrow(data)-1,], mapping=aes(x=dateRep[1:nrow(data)-1], y=cases[1:nrow(data)-1]), color=top_colors[i],size=1.08)+ggtitle(label=top_countries[i])+ylim(0,65000)+scale_x_date(breaks = scales::breaks_pretty(10))+labs(x="Time",y="New cases")
}
ggsave("new_daily_cases_top_5_countries.png", plot=do.call("grid.arrange",c(plt_cases, ncol=1)), width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# then i do the same things with deaths data
# create a column with cumulative sum of deaths
deaths_sum <- vector("numeric", length=nrow(cases_c))
j <- nrow(cases_c)
for(i in 1:length(countries_cases)){
	deaths_sum[j] <- cases_c$deaths[j]
	j <- j-1
	repeat{
		deaths_sum[j] <- deaths_sum[j+1] + cases_c$deaths[j]
		j <- j-1
		if(!strcmp(cases_c$countriesAndTerritories[j],cases_c$countriesAndTerritories[j+1])){
			break
		}
	}	
}

cases_c<-cbind(cases_c, deaths_sum)
cases_c<-cases_c[,c(1,2,3,4,8,5,6,7)]

# total deaths plot
p3 <- ggplot()
for(i in 1:length(top_countries)){
	p3 <- p3 + geom_line(data=filter(cases_c, countriesAndTerritories==top_countries[i]), aes(x=dateRep, y=deaths_sum), color=top_colors[i], size=1.08)
}

p3<-p3+geom_text(aes(label=top_countries, x=dateRep[ind3], y=deaths_sum[ind3]), color=top_colors, hjust=-.1)
p3<-p3+scale_x_date(breaks = scales::breaks_pretty(10), limits=c(dateRep[length(dateRep)],as.Date("2021-11-15")))
p3<-p3+ggtitle(label="Evolution of total deaths per EU country in top 5 countries")+labs(x="Time",y="Total deaths")
ggsave("deaths_evolution_top_5_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# daily deaths plot
plt_deaths <- list()
for(i in 1:5){
	data <- filter(cases_c, countriesAndTerritories==top_countries[i])
	plt_deaths[[i]]<-ggplot()+geom_line(data[1:nrow(data)-1,],mapping=aes(x=dateRep[1:nrow(data)-1], y=deaths[1:nrow(data)-1]),color=top_colors[i],size=1.08)+ggtitle(label=top_countries[i])+scale_x_date(breaks = scales::breaks_pretty(10))+ylim(0,1000)
}
ggsave("new_daily_deaths_top_5_countries.png", plot=do.call("grid.arrange",c(plt_deaths, ncol=1)), width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)


## VACCINES DATASET ##

# data prep (summarizing data of single vaccine doses filtering by ALL age group to avoid duplicates)
temp1 <- filter(vaccine_mono, TargetGroup=="ALL")
temp2 <- filter(vaccine_double, TargetGroup=="ALL")
temp3 <- filter(vaccine_c, TargetGroup=="ALL")

vaccine_mono_tot <- as.data.frame(summarize(group_by(temp1, ReportingCountry),sum(FirstDose), sum(NumberDosesReceived), unique(Population)))
vaccine_double_tot <- as.data.frame(summarize(group_by(temp2, ReportingCountry),sum(FirstDose), sum(SecondDose), sum(UnknownDose), sum(NumberDosesReceived), unique(Population)))
vaccine_tot <- as.data.frame(summarize(group_by(temp3, ReportingCountry),sum(FirstDose), sum(SecondDose), sum(UnknownDose), sum(NumberDosesReceived), unique(Population)))

rm(temp1,temp2)

colnames(vaccine_mono_tot) <- c("ReportingCountry","FirstDose","NumberDosesReceived","Population")
colnames(vaccine_double_tot) <- c("ReportingCountry","FirstDose","SecondDose","UnknownDose","NumberDosesReceived","Population")
colnames(vaccine_tot) <- c("ReportingCountry","FirstDose","SecondDose","UnknownDose","NumberDosesReceived","Population")

# add SE country to vaccine_mono_tot
temp2 <- data.frame("SE",0,0,10327589)
names(temp2)<-c("ReportingCountry","FirstDose","NumberDosesReceived","Population")
vaccine_mono_tot <- rbind(vaccine_mono_tot,temp2)
vaccine_mono_tot <- arrange(vaccine_mono_tot,ReportingCountry)

# total first doses (first of double doses)
ggplot()+geom_bar(vaccine_double_tot,mapping=aes(x=reorder(ReportingCountry,-FirstDose),y=FirstDose,fill=FirstDose),stat="identity")+geom_text(vaccine_double_tot,mapping=aes(x=ReportingCountry,y=FirstDose,label=FirstDose), vjust=-0.9, position = position_dodge(0.9), size=3.5)+labs(x="Countries",y="First doses")+ggtitle(label="Total covid vaccines' first doses administered in EU countries")
ggsave("total_vaccines_first_doses_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# total second doses (second of double doses)
ggplot()+geom_bar(vaccine_double_tot,mapping=aes(x=reorder(ReportingCountry,-SecondDose),y=SecondDose,fill=SecondDose),stat="identity")+geom_text(vaccine_double_tot,mapping=aes(x=ReportingCountry,y=SecondDose,label=SecondDose), vjust=-0.9, position = position_dodge(0.9), size=3.5)+labs(x="Countries",y="Second doses")+ggtitle(label="Total covid vaccines' second doses administered in EU countries")
ggsave("total_vaccines_second_doses_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# total single doses (single dose vaccine)
ggplot()+geom_bar(vaccine_mono_tot,mapping=aes(x=reorder(ReportingCountry,-FirstDose),y=FirstDose,fill=FirstDose),stat="identity")+geom_text(vaccine_mono_tot,mapping=aes(x=ReportingCountry,y=FirstDose,label=FirstDose), vjust=-0.9, position = position_dodge(0.9), size=3.5)+labs(x="Countries",y="Doses")+ggtitle(label="Total covid mono-dose vaccine's doses administered in EU countries")
ggsave("total_vaccines_single_doses_by_country.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# total doses (first + second + single)
a <- select(vaccine_double_tot, ReportingCountry, FirstDose)
b <- select(vaccine_double_tot, ReportingCountry, SecondDose)
c <- select(vaccine_mono_tot, ReportingCountry, FirstDose)
colnames(c)<-c("ReportingCountry","MonoDose")
bars <- merge(a,b)
bars <- merge(bars,c)
temp <- tidyr::pivot_longer(bars, cols=c("FirstDose","SecondDose","MonoDose"),names_to="dose",values_to="doses")

tot <- temp %>% group_by(ReportingCountry) %>% summarize(total = sum(doses))

ggplot(temp, aes(x=reorder(ReportingCountry,-doses), y=doses, fill=dose))+geom_bar(stat="identity",position="stack")+labs(x="Countries",y="Doses")+ggtitle(label="Total covid vaccines' doses administered in EU countries")+geom_text(tot,mapping=aes(ReportingCountry, total, label = total, fill = NULL),vjust=-0.9)
ggsave("total_vaccines_doses_by_country.png", width=2120, height=1080, units="px", path=IMAGE_PATH, scale=3)

# percentages over total population

percPrimaDose <- vector("numeric", length=nrow(vaccine_double_tot))
for(i in 1:nrow(vaccine_double_tot)){
	percPrimaDose[i] <- (vaccine_double_tot$FirstDose[i]/vaccine_double_tot$Population[i])*100
}

percSecondaDose <- vector("numeric", length=nrow(vaccine_double_tot))
for(i in 1:nrow(vaccine_double_tot)){
	percSecondaDose[i] <- (vaccine_double_tot$SecondDose[i]/vaccine_double_tot$Population[i])*100
}

percMonoDose <- vector("numeric", length=nrow(vaccine_mono_tot))
for(i in 1:nrow(vaccine_mono_tot)){
	percMonoDose[i] <- (vaccine_mono_tot$FirstDose[i]/vaccine_mono_tot$Population[i])*100
}

vaccine_tot <- cbind(vaccine_tot, percPrimaDose, percSecondaDose, percMonoDose)

# percentages bar plot
d <- select(vaccine_tot, ReportingCountry, percPrimaDose)
e <- select(vaccine_tot, ReportingCountry, percSecondaDose)
f <- select(vaccine_tot, ReportingCountry, percMonoDose)
bars2 <- merge(d,e)
bars2 <- merge(bars2,f)
temp2 <- tidyr::pivot_longer(bars2, cols=c("percPrimaDose","percSecondaDose","percMonoDose"),names_to="dose",values_to="percs")
ggplot(temp2, aes(x=reorder(ReportingCountry,-percs), y=percs, fill=dose))+geom_bar(stat="identity",position="dodge")+labs(x="Countries",y="Vaccine inoculation (%)")+ggtitle(label="Percentages of vaccines' doses administered in EU countries")
ggsave("vaccine_percentages_bar_plot.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# same ordered by first dose
percPrima <- vector("numeric", length=90)
for(i in seq(1,nrow(temp2),by=3)){
	percPrima[i] <- temp2$percs[i]
}
temp2 <- cbind(temp2, percPrima)
ggplot(temp2, aes(x=reorder(ReportingCountry,-percPrima), y=percs, fill=dose))+geom_bar(stat="identity",position="dodge")+labs(x="Countries",y="Vaccine inoculation (%)")+ggtitle(label="Percentages of vaccines' doses administered in EU countries")
ggsave("vaccine_percentages_bar_plot_ordered.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# second dose + single dose percentages to see percentages of people who completed the vaccine cycle
bars3 <- merge(e,f)
temp3 <- tidyr::pivot_longer(bars2, cols=c("percSecondaDose","percMonoDose"),names_to="dose",values_to="percs")
# for the tags
totals <- temp3 %>% group_by(ReportingCountry) %>% summarize(total = sum(percs))

ggplot(temp3,aes(x=reorder(ReportingCountry,-percs), y=percs, fill=dose))+geom_bar(stat="identity",position="stack")+labs(x="Countries",y="Vaccine inoculation (%)")+ggtitle(label="Percentages of people who completed the vaccination cycle in EU countries")+geom_text(totals,mapping=aes(x=ReportingCountry, y=total, label=sprintf("%.02f%%",round(total,digits=2)),vjust=-0.9,fill=NULL))
ggsave("vaccine_complete_cycle_percentage_bar_plot.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)


# percentage of doses used to those received
# received doses data was not available for every country
percReceived <- vector("numeric", length=sum(!is.na(vaccine_tot$NumberDosesReceived)))
j <- 1
for(i in 1:nrow(vaccine_tot)){
	if(!is.na(vaccine_tot$NumberDosesReceived[i])){
		percReceived[j] <- ((vaccine_tot$FirstDose[i]+vaccine_tot$SecondDose[i])/vaccine_tot$NumberDosesReceived[i])*100
		j <- j+1
	}
}
# plot
perc_received_df <- filter(vaccine_tot, !is.na(NumberDosesReceived))
perc_received_df <- select(perc_received_df, ReportingCountry)
perc_received_df <- cbind(perc_received_df, percReceived)
ggplot() + geom_bar(perc_received_df, mapping=aes(x=reorder(ReportingCountry,-percReceived), y=percReceived, fill=percReceived), stat="identity", position="dodge")+geom_text(perc_received_df,mapping=aes(x=ReportingCountry,y=percReceived,label=sprintf("%0.2f%%",round(percReceived,digits=2))), position=position_dodge(width=0.9), vjust=-0.25)+labs(x="Countries",y="Doses used (%)")+ggtitle(label="Percentages of doses used to received by EU countries")
ggsave("vaccine_percentage_doses_used_to_received.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# age groups data
#countries divided by age group
vaccine_age_c <- subset(vaccine_age_c,!(ReportingCountry_age%in%c("DE","LI","NL")))
# ordering countries
countries_age <- arrange(vaccine_tot, -(FirstDose+SecondDose))$ReportingCountry
countries_age <- countries_age[-c(1,6,30)] # DE LI e NL have to be removed because they don't have age groups data
# first dose
plt_age <- list()
for(i in 1:length(countries_age)){
    data <- filter(vaccine_age_c, ReportingCountry_age==countries_age[i])
    plt_age[[i]] <- ggplot(data,aes(TargetGroup,FirstDose_age))+geom_bar(stat="identity",aes(fill=TargetGroup),position="dodge")+theme(legend.position = "none",axis.text.x =element_text(angle=30, hjust=1),axis.title.x = element_blank())+ggtitle(label=countries_age[i])+labs(y="Doses")
    if(!i%in%c(1,10,19)){
        plt_age[[i]]<-plt_age[[i]]+theme(axis.title.y=element_blank())
    }
}
ggsave("vaccine_first_dose_age_groups.png", plot=do.call("grid.arrange",c(plt_age, ncol=9)), width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# second dose
plt_age <- list()
for(i in 1:length(countries_age)){
    data <- filter(vaccine_age_c, ReportingCountry_age==countries_age[i])
    plt_age[[i]] <- ggplot(data,aes(TargetGroup,SecondDose_age))+geom_bar(stat="identity",aes(fill=TargetGroup),position="dodge")+theme(legend.position = "none",axis.text.x =element_text(angle=30, hjust=1),axis.title.x = element_blank())+ggtitle(label=countries_age[i])+labs(y="Doses")
    if(!i%in%c(1,10,19)){
        plt_age[[i]]<-plt_age[[i]]+theme(axis.title.y=element_blank())
    }
}
ggsave("vaccine_second_dose_age_groups.png", plot=do.call("grid.arrange",c(plt_age, ncol=9)), width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)


# time evolution 
plt_time <- list()
vax <- unique(vaccine_c$Vaccine)

vaccine_time <- as.data.frame(vaccine_c %>% group_by(ReportingCountry, YearWeekISO, Vaccine)%>%summarise(FirstDose=sum(FirstDose),SecondDose=sum(SecondDose)))
color_vax <- c("#ff2e58","#ffb012","#308a29","#2e7bff","#6efaff","#fea6ff","#b4ff85")

for(i in 1:length(countries)){
	data <- filter(vaccine_time, ReportingCountry==countries[i])
	plt_time[[i]] <- ggplot()
	for(j in 1:length(vax)){
		if(vax[j]%in%unique(data$Vaccine)){
			data2 <- filter(data, Vaccine==vax[[j]])
			plt_time[[i]] <- plt_time[[i]] + geom_line(data2, mapping=aes(YearWeekISO,FirstDose,group=1), color=color_vax[j], size=1.08)+theme(axis.text.x =element_text(angle=90, hjust=1))
		}
	}
	plt_time[[i]] <- plt_time[[i]] + ggtitle(label=countries[i])
	if(!(i%in%c(1,4,7,10,13,16,19,22,25,28))){
		plt_time[[i]] <- plt_time[[i]] + theme(axis.title.y=element_blank())
	}
}
ggsave("vaccine_brands_usage.png", plot=do.call("grid.arrange",c(plt_time, ncol=3)), width=1920, height=4800, units="px", path=IMAGE_PATH, scale=3)

###########################################################################################

### CASES - VACCINES COMPARISON ###

###########################################################################################

# vaccine dataset prep
# divide first dose - final dose (second + single)
FirstDose <- vector("numeric",length=nrow(vaccine_time))
FinalDose <- vector("numeric",length=nrow(vaccine_time))
for(i in 1:nrow(vaccine_time)){
	if(vaccine_time$Vaccine[i]=="JANSS"){
		FirstDose[i]<-0
		FinalDose[i]<-vaccine_time$FirstDose[i]
	} else {
		FirstDose[i]<-vaccine_time$FirstDose[i]
		FinalDose[i]<-vaccine_time$SecondDose[i]
	}
}
temp <- cbind(vaccine_time, FirstDose, FinalDose)
temp <- temp[,c(2,1,3,6,5,7)]

# group by weeks
vaccine_vs <- as.data.frame(summarize(group_by(temp,ReportingCountry,YearWeekISO),sum(FirstDose),sum(SecondDose),sum(FinalDose)))
colnames(vaccine_vs) <- c("ReportingCountry","YearWeekISO","FirstDose","SecondDose","FinalDose")

# need to reorder countries
countries_vax <- unique(vaccine_vs$ReportingCountry)
countries_cas <- unique(cases_weekly_tot$countriesAndTerritories)
countries_vax <- countries_vax[c(1,2,3,13,4,5,7,8,11,12,6,9,14,16,15,17,21,18,19,20,22,23,24,25,26,27,30,29,10,28)]

# mortality rate
mortalityRate <- vector("numeric", length = nrow(cases_weekly_tot))
for(i in 1:nrow(cases_weekly_tot)){
	mortalityRate[i] <- (cases_weekly_tot$deaths[i]/cases_weekly_tot$cases[i])*100
}
cases_weekly_tot <- cbind(cases_weekly_tot, mortalityRate)

plt_comp <- list()

ind <- 1
for(i in seq(1,length(countries_vax),by=3)){
	for(j in 0:2){
		data_vax <- filter(vaccine_vs, ReportingCountry==countries_vax[i+j])
		data_cas <- filter(cases_weekly_tot, countriesAndTerritories==countries_cas[i+j])
		data <- merge(data_vax,data_cas,by="YearWeekISO")
		data <- filter(data, YearWeekISO!="2021-W09"&YearWeekISO!="2021-W08")
		plt_comp[[ind+j]] <- ggplot()+
			geom_line(data,mapping=aes(x=YearWeekISO,y=FirstDose),group=1,color="#c7d6c5")+
			geom_line(data,mapping=aes(x=YearWeekISO,y=SecondDose),group=1,color="#43bf49")+
			geom_line(data,mapping=aes(x=YearWeekISO,y=FinalDose),group=1,size=1.08,color="#43bf49")+
			ggtitle(label=paste0(countries_cas[i+j]," - Vaccinations"))+
			labs(x="Time (weeks)",y="Doses")+theme(axis.text.x =element_text(angle=45, hjust=1))
		plt_comp[[ind+j+3]] <- ggplot()+
			geom_line(data,mapping=aes(x=YearWeekISO,y=mortalityRate),group=1,size=1.08,color="#d1000a")+
			ggtitle(label=paste0(countries_cas[i+j]," - Mortality rate (%)"))+
			labs(x="Time (weeks)",y="Mortality rate (%)")+theme(axis.text.x =element_text(angle=45, hjust=1))
	}
	ind <- ind+6
}
ggsave("vaccine_percentage_and_covid_mortality_comparison.png", plot=do.call("grid.arrange",c(plt_comp, ncol=3)), width=1920, height=4800, units="px", path=IMAGE_PATH, scale=3)

# covid deaths per thousand and fully vaccinated people percentage comparison
# ordering countries
vax_tot <- vaccine_tot[c(1,2,3,13,4,5,7,8,11,12,6,9,14,16,15,17,21,18,19,20,22,23,24,25,26,27,30,29,10,28),]
percCompleti <- vax_tot$percSecondaDose + vax_tot$percMonoDose
vax_tot <- cbind(vax_tot, percCompleti)

cases_vax_tot <- filter(cases_weekly_tot, YearWeekISO!="2021-W09")
cases_vax_tot <- as.data.frame(summarize(group_by(cases_vax_tot,countriesAndTerritories),unique(countryterritoryCode),sum(cases),sum(deaths),unique(popData2020)))
colnames(cases_vax_tot)<-c("countriesAndTerritories","countryterritoryCode","cases","deaths","popData2020")

deathsPerMille <- vector("numeric", length = nrow(cases_vax_tot))
for(i in 1:nrow(cases_vax_tot)){
	deathsPerMille[i] <- cases_vax_tot$deaths[i]/(cases_vax_tot$popData2020[i]/1000)
}

mortalityRate <- vector("numeric", length = nrow(cases_vax_tot))
for(i in 1:nrow(cases_vax_tot)){
	mortalityRate[i] <- (cases_vax_tot$deaths[i]/cases_vax_tot$cases[i])*100
}

casesPerMille <- vector("numeric", length=nrow(cases_vax_tot))
for(i in 1:nrow(cases_vax_tot)){
	casesPerMille[i] <- cases_vax_tot$cases[i]/(cases_vax_tot$popData2020[i]/1000)
}

cases_vax_tot <- cbind(cases_vax_tot, deathsPerMille, mortalityRate, casesPerMille)
vaxcases <- cbind(vax_tot,cases_vax_tot)

ggplot(vaxcases,aes(percSecondaDose, deathsPerMille))+geom_point(stat="identity",position="dodge", aes(colour=countriesAndTerritories),size=3) + ggtitle(label="Deaths per thousand to percentage of people who have completed the vaccination cycle")+geom_text_repel(aes(label=countryterritoryCode))+labs(x="Percentage of people who completed the vaccination cycle", y="Deaths per thousand")+geom_smooth(method="lm",se=FALSE)+theme(legend.position="none")
ggsave("deaths_per_thousand_and_fully_vaccinated_percentage.png", width=1080, height=1080, units="px", path=IMAGE_PATH, scale=3)

# pre and post vaccination campaign comparison
cases_preVax <- cases_c[FALSE,]

for(i in 1:length(countries_cases)){
    data <- filter(cases_c, countriesAndTerritories==countries_cases[i])
		cases_preVax <- rbind(cases_preVax, tail(data,n=1))    
}

deathsPerMille <- vector("numeric", length = nrow(cases_preVax))
for(i in 1:nrow(cases_preVax)){
	deathsPerMille[i] <- cases_preVax$deaths[i]/(cases_preVax$popData2020[i]/1000)
}

mortalityRate <- vector("numeric", length = nrow(cases_preVax))
for(i in 1:nrow(cases_preVax)){
	mortalityRate[i] <- (cases_preVax$deaths[i]/cases_preVax$cases[i])*100
}

casesPerMille <- vector("numeric", length=nrow(cases_preVax))
for(i in 1:nrow(cases_preVax)){
	casesPerMille[i] <- cases_preVax$cases[i]/(cases_preVax$popData2020[i]/1000)
}
cases_preVax <- cbind(cases_preVax, deathsPerMille, mortalityRate, casesPerMille)
cases_preVax <- cases_preVax[,c(2,4,6,7,8,9,10,11)]

ggplot(cases_preVax ,aes(casesPerMille, deathsPerMille))+geom_point(stat="identity",position="dodge", aes(colour=countriesAndTerritories),size=3) + ggtitle(label="Deaths per thousand to cases per thousand")+geom_text_repel(aes(label=countryterritoryCode))+labs(x="Cases per thousand", y="Deaths per thousand")+theme(legend.position="none")
ggsave("deaths_per_thousand_to_cases_per_thousand_PRE.png", width=1080, height=1080, units="px", path=IMAGE_PATH, scale=3)
ggplot(vaxcases ,aes(casesPerMille, deathsPerMille))+geom_point(stat="identity",position="dodge", aes(colour=countriesAndTerritories),size=3) + ggtitle(label="Deaths per thousand to cases per thousand")+geom_text_repel(aes(label=countryterritoryCode))+labs(x="Cases per thousand", y="Deaths per thousand")+theme(legend.position="none")
ggsave("deaths_per_thousand_to_cases_per_thousand_POST.png", width=1080, height=1080, units="px", path=IMAGE_PATH, scale=3)

# pre and post comparison with arrows
p <- ggplot()+geom_point(cases_preVax,stat="identity",position="dodge",mapping=aes(casesPerMille, deathsPerMille),color="red",size=3) + ggtitle(label="Deaths per thousand to cases per thousand")+geom_text(cases_preVax,mapping=aes(casesPerMille, deathsPerMille,label=countryterritoryCode), vjust=-0.6)+labs(x="Cases per thousand", y="Deaths per thousand")
p <- p+geom_point(vaxcases,stat="identity",position="dodge",mapping=aes(casesPerMille, deathsPerMille),color="blue",size=3) + ggtitle(label="Deaths per thousand to cases per thousand")+geom_text(cases_preVax,mapping=aes(casesPerMille, deathsPerMille,label=countryterritoryCode), vjust=-0.6)+labs(x="Cases per thousand", y="Deaths per thousand")

cases_postVax <- cases_vax_tot[,c(3,4,1,2,5,6,7,8)]

prePost <- rep("pre",30)
cases_preVax <- cbind(cases_preVax, prePost)
prePost <- rep("post",30)
cases_postVax <- cbind(cases_postVax, prePost)

cases_prePost <- rbind(cases_preVax,cases_postVax)

ggplot()+geom_point(cases_prePost,stat="identity",position="dodge",mapping=aes(casesPerMille, deathsPerMille, color=prePost),size=4) + ggtitle(label="Deaths per thousand to cases per thousand")+geom_text_repel(cases_prePost,mapping=aes(casesPerMille, deathsPerMille,label=countryterritoryCode))+labs(x="Cases per thousand", y="Deaths per thousand")+scale_color_manual(values = c("pre" = "#ff616e", "post" = "#739957"))+geom_path(cases_prePost,mapping=aes(casesPerMille, deathsPerMille,group=countriesAndTerritories),color="#363636",arrow = arrow(type = "closed",length=unit(0.075, "inches")))+theme(legend.position="none")
ggsave("deaths_per_thousand_to_cases_per_thousand_PRE_AND_POST.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

# geographic data and plots

##################################################################

# *** WARNING! *** generating maps requires a Google API key!

##################################################################

# register_google(key="***INSERT KEY HERE***")
# eumap <- qmap("Europe",zoom=4,color="bw")
# icemap <- qmap("Iceland",zoom=4,color="bw")
# 
# # for cases
# latlon <- geocode(cases_tot$countriesAndTerritories)
# cases_tot_geo <- cbind(cases_tot,latlon)
# vax_tot_geo <- cbind(vax_tot, latlon)
# 
# # cases per mille
# eumap+geom_point(cases_tot_geo,mapping=aes(x=lon, y=lat, color=casesPerMille), size=casesPerMille/4)+scale_color_gradient2(mid="red",high="yellow",limits=c(30,170))
# ggsave("europe_map_cases_per_thousand.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)
# icemap+geom_point(cases_tot_geo,mapping=aes(x=lon, y=lat, color=casesPerMille), size=casesPerMille/4)+scale_color_gradient2(mid="red",high="yellow",limits=c(30,170))
# ggsave("iceland_map_cases_per_thousand.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)
# 
# # deaths per mille
# eumap+geom_point(cases_tot_geo,mapping=aes(x=lon, y=lat, color=deathsPerMille), size=deathsPerMille*10)+scale_color_gradient2(mid="red",high="yellow")
# ggsave("europe_map_deaths_per_thousand.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)
# icemap+geom_point(cases_tot_geo,mapping=aes(x=lon, y=lat, color=deathsPerMille), size=deathsPerMille*10)+scale_color_gradient2(mid="red",high="yellow")
# ggsave("iceland_map_deaths_per_thousand.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)
# 
# # perc of total vaxed people
# eumap+geom_point(vax_tot_geo,mapping=aes(x=lon, y=lat, color=percCompleti), size=percCompleti/3)+scale_color_gradient2(mid="red",high="yellow",limits=c(0,100))
# ggsave("europe_map_vaccinated_people_percentage.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)
# icemap+geom_point(vax_tot_geo,mapping=aes(x=lon, y=lat, color=percCompleti), size=percCompleti/3)+scale_color_gradient2(mid="red",high="yellow",limits=c(0,100))
# ggsave("iceland_map_vaccinated_people_percentage.png", width=1920, height=1080, units="px", path=IMAGE_PATH, scale=3)

