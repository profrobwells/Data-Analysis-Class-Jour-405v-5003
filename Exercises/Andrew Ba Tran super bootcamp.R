

df4 <- filter(murders, State=="Arkansas", Solved_label=="No", Year==2016)

#VICRACE

murders$VicRace_label

whitevics <- filter(murders, State=="Arkansas", VicRace_label=="White", Solved_label=="No", Year==2016)

#Asian or Pacific Islander
asianvics <- filter(murders, State=="Arkansas", VicRace_label=="Asian or Pacific Islander", Year==2016)

#multiple results
#%in% c("Husband", "Boyfriend")
unsolvedwhiteblack <- filter(murders, State=="Arkansas", VicRace_label %in% c("White", "Black"), Solved_label=="No", Year==2016)


#NWA counties - Benton, Washington, Sebastian, Newton
unsolvedwhiteblack <- filter(murders, State=="Arkansas", VicRace_label %in% c("White", "Black"), Solved_label=="No", Year==2016)

counties2 <- filter(murders, State=="Arkansas", MSA_label=="Fayetteville-Springdale-Rogers, AR-MO", Year==2016)

murders$Agency
