## read in original incomplete data file - please change pathway to as required 

## alternatively jump to line 1642 and add appropriate commands to read in completed data file or subsequent train and test files (and beware of redundant first column - see github 'read me' for details)

path = "D://Project_pt5.csv"

tennis = read.csv(path)

rm(list = ls())


# make sure input file has sets breakdown appearing as text and not dates (necessary to check for original incomplete data file only, for calculation of tie break data) 

# to change factor players names to strings (needed for doing some of calcs below)

for(i in 1:nrow(tennis)){
  tennis[i,11] = as.character(tennis[i,10])
}

for(i in 1:nrow(tennis)){
  tennis[i,15] = as.character(tennis[i,16])
}

## now remove defunct columns 

tennis = tennis[,-10]
tennis = tennis[,-15]

# to reverse in tournament sequence for 2019 data as it was in different seqeunces for earlier years, and needs to be the same for back stats

s = 1
tc = c()

for (i in 1:2530){
  if(tennis[i+1,3] == tennis[i,3]){
    s=s+1
    i = i +1 
  } else{
    tc = append(tc,s)
    s=1
    i = i + 1
  }
}  


length(tc)

tennis1 = tennis

for(i in 1:1){
  for(j in 1:tc[i]){
    tennis1[j,] = tennis[(tc[i]+1 - j), ]
  }
}

k = 15

for(i in 2:66){
  c = 1
  for(j in k:(tc[i]+k -1)){
    tennis1[j,] = tennis[(tc[i] + k - c), ]
    c = c + 1
  }
  k = tc[i] + k
}

tennis = tennis1



# find and remove walkovers

wo =  grep("W/O", tennis[,19])

wo

for (i in 1:length(wo)){
  tennis = tennis[-wo[1],]
  wo =  grep("W/O", tennis[,19])
}

# reset row numbers 

row.names(tennis) = NULL

# to assign value to grand slam tournament level

for (i in 1:nrow(tennis)){
  if (tennis[i,8] == "A"){
    tennis[i,9] = 0
  }
  else{
    tennis[i,9] = 1
  }
}

# to assign temporary values to tournament rounds

for (i in 1:nrow(tennis)){
  if (tennis[i,20] == "F"){
    tennis[i,21] = 1
  }
  if (tennis[i,20] == "SF"){
    tennis[i,21] = 2
  }
  if (tennis[i,20] == "QF"){
    tennis[i,21] = 3
  }
  if (tennis[i,20] == "R16"){
    tennis[i,21] = 4
  }
  if (tennis[i,20] == "R32"){
    tennis[i,21] = 5
  }
  if (tennis[i,20] == "R64"){
    tennis[i,21] = 6
  }
  if (tennis[i,20] == "R128"){
    tennis[i,21] = 7
  }
  if (tennis[i,20] == "RR"){
    tennis[i,21] = 3
  }
}

# to assign '1' value to relevant surface 

for (i in 1:nrow(tennis)){
  tennis[i,5] = 0
  tennis[i,6] = 0
  tennis[i,7] = 0
  if (tennis[i,4] == "Hard"){
    tennis[i,5] = 1
  }
  if (tennis[i,4] == "Clay"){
    tennis[i,6] = 1
  }
  if (tennis[i,4] == "Grass"){
    tennis[i,7] = 1
  }
}


# impute mean for missing mins data and segregate between GS and other if statistical difference

tennis[,22] = replace(tennis[,22],is.na(tennis[,22]),0)


gsl = c()
atpl = c()
gcounter = 0
acounter =0

for (i in 1:7829){ 
  if((tennis[i,22] > 0) & (tennis[i,9] == 1)){
    gcounter = gcounter +1
    gsl[gcounter] = tennis[i,22]
  }
  if((tennis[i,22] > 0) & (tennis[i,9] == 0)){
    acounter = acounter +1
    atpl[acounter] = tennis[i,22]
  }
}

t.test(gsl,atpl,alternative = "two.sided", var.equal = FALSE)

### impute separate means for missing mins data in grand slams than in other tournaments

for (i in 1:7829){
  if((tennis[i,22] == 0) & (tennis[i,9] == 1)){
    tennis[i,22] = sum(gsl)/length(gsl)
  }
  if((tennis[i,22] == 0) & (tennis[i,9] == 0)){
    tennis[i,22] = sum(atpl)/length(atpl)
  }
}



# to calculate back minutes played in tournamnent (fatigue factor) 

for (i in 1:7607){

  j = i
  tennis[j,23] = 0
  tennis[j,24] = 0
  if (tennis[i,21] == 1){
    k = i
  }

  while ((tennis[i+1,21] > tennis[k,21]) & ((i+1) < 7830)){
    if (tennis [i+1, 10] == tennis[j,10]){
      tennis[j,23] = tennis[j,23] + tennis[i+1,22]
    }
    if (tennis [i+1, 14] == tennis[j,10]){
      tennis[j,23] = tennis[j,23] + tennis[i+1,22]
    }
    if (tennis [i+1, 10] == tennis[j,14]){
      tennis[j,24] = tennis[j,24] + tennis[i+1,22]
    }
    if (tennis [i+1, 14] == tennis[j,14]){
      tennis[j,24] = tennis[j,24] + tennis[i+1,22]
    }
    i = i + 1
  } 
}



# now find and remove retirements (should be post fatigue calc above)

ret =  grep("RET", tennis[,19])

ret

for (i in 1:length(ret)){
  tennis = tennis[-ret[1],]
  ret =  grep("RET", tennis[,19])
}


# find and remove rows with NA's for main match data 

tennis[,25] = replace(tennis[,25],is.na(tennis[,25]),0)

i = 1

while (i <= length(row.names(tennis))){
  if (tennis[i,25] == 0){
    tennis = tennis[-i,]
  }
  else{
    i = i + 1
  }
}



# find rows with NA's for betting odds and replace with 0's 

tennis[,141] = replace(tennis[,141],is.na(tennis[,141]),0)

tennis[,142] = replace(tennis[,142],is.na(tennis[,142]),0)


# reset row numbers 

row.names(tennis) = NULL


# to set every second game as win or lose for later reference 

for (i in 1:nrow(tennis)){
  if((i%%2)==0){
    tennis[i,101] = "L"
  }
  else{
    tennis[i,101] = "W"
  }
}


# to calculate a range of average back statistics against all players and on all surfaces 

for (j in 1:7607){
  
  games = 0
  gamesw = 0
  servepc = 0
  returnpc = 0
  sgameswpc = 0
  rgameswpc = 0
  tbw = c()
  tbl = c()
  
  games1 = 0
  gamesw1 = 0
  servepc1 = 0
  returnpc1 = 0
  sgameswpc1 = 0
  rgameswpc1 = 0
  tbw1 = c()
  tbl1 = c()
  
  
  for (i in j:(j+5081)){
    
    if (tennis[i+1,10] == tennis[j,10]){
      servepc = servepc + as.numeric(paste(tennis[i+1,33]))
      returnpc = returnpc + as.numeric(paste(tennis[i+1,34]))
      sgameswpc = sgameswpc + as.numeric(paste(tennis[i+1,35]))
      rgameswpc = rgameswpc + as.numeric(paste(tennis[i+1,36]))
      games = games + 1
      gamesw = gamesw + 1
      tbw = append(tbw, grep("7-6", tennis[i+1,116]))
      tbw = append(tbw, grep("7-6", tennis[i+1,117]))
      tbw = append(tbw, grep("7-6", tennis[i+1,118]))
      tbw = append(tbw, grep("7-6", tennis[i+1,119]))
      tbw = append(tbw, grep("7-6", tennis[i+1,120]))
      tbw = append(tbw, grep("13-12", tennis[i+1,120]))
      tbl = append(tbl, grep("6-7", tennis[i+1,116]))
      tbl = append(tbl, grep("6-7", tennis[i+1,117]))
      tbl = append(tbl, grep("6-7", tennis[i+1,118]))
      tbl = append(tbl, grep("6-7", tennis[i+1,119]))
      tbl = append(tbl, grep("6-7", tennis[i+1,120]))
      tbl = append(tbl, grep("12-13", tennis[i+1,120]))
    }
    
    if (tennis[i+1,14] == tennis[j,10]){
      servepc = servepc + as.numeric(paste(tennis[i+1,65]))
      returnpc = returnpc + as.numeric(paste(tennis[i+1,66]))
      sgameswpc = sgameswpc + as.numeric(paste(tennis[i+1,67]))
      rgameswpc = rgameswpc + as.numeric(paste(tennis[i+1,68]))
      games = games + 1
      tbl = append(tbl, grep("7-6", tennis[i+1,116]))
      tbl = append(tbl, grep("7-6", tennis[i+1,117]))
      tbl = append(tbl, grep("7-6", tennis[i+1,118]))
      tbl = append(tbl, grep("7-6", tennis[i+1,119]))
      tbl = append(tbl, grep("7-6", tennis[i+1,120]))
      tbl = append(tbl, grep("13-12", tennis[i+1,120]))
      tbw = append(tbw, grep("6-7", tennis[i+1,116]))
      tbw = append(tbw, grep("6-7", tennis[i+1,117]))
      tbw = append(tbw, grep("6-7", tennis[i+1,118]))
      tbw = append(tbw, grep("6-7", tennis[i+1,119]))
      tbw = append(tbw, grep("6-7", tennis[i+1,120]))
      tbw = append(tbw, grep("12-13", tennis[i+1,120]))
    }
    
    
    if (tennis[i+1,10] == tennis[j,14]){
      servepc1 = servepc1 + as.numeric(paste(tennis[i+1,33]))
      returnpc1 = returnpc1 + as.numeric(paste(tennis[i+1,34]))
      sgameswpc1 = sgameswpc1 + as.numeric(paste(tennis[i+1,35]))
      rgameswpc1 = rgameswpc1 + as.numeric(paste(tennis[i+1,36]))
      games1 = games1 + 1
      gamesw1 = gamesw1 + 1
      tbw1 = append(tbw1, grep("7-6", tennis[i+1,116]))
      tbw1 = append(tbw1, grep("7-6", tennis[i+1,117]))
      tbw1 = append(tbw1, grep("7-6", tennis[i+1,118]))
      tbw1 = append(tbw1, grep("7-6", tennis[i+1,119]))
      tbw1 = append(tbw1, grep("7-6", tennis[i+1,120]))
      tbw1 = append(tbw1, grep("13-12", tennis[i+1,120]))
      tbl1 = append(tbl1, grep("6-7", tennis[i+1,116]))
      tbl1 = append(tbl1, grep("6-7", tennis[i+1,117]))
      tbl1 = append(tbl1, grep("6-7", tennis[i+1,118]))
      tbl1 = append(tbl1, grep("6-7", tennis[i+1,119]))
      tbl1 = append(tbl1, grep("6-7", tennis[i+1,120]))
      tbl1 = append(tbl1, grep("12-13", tennis[i+1,120]))
    }
    
    if (tennis[i+1,14] == tennis[j,14]){
      servepc1 = servepc1 + as.numeric(paste(tennis[i+1,65]))
      returnpc1 = returnpc1 + as.numeric(paste(tennis[i+1,66]))
      sgameswpc1 = sgameswpc1 + as.numeric(paste(tennis[i+1,67]))
      rgameswpc1 = rgameswpc1 + as.numeric(paste(tennis[i+1,68]))
      games1 = games1 + 1
      tbl1 = append(tbl1, grep("7-6", tennis[i+1,116]))
      tbl1 = append(tbl1, grep("7-6", tennis[i+1,117]))
      tbl1 = append(tbl1, grep("7-6", tennis[i+1,118]))
      tbl1 = append(tbl1, grep("7-6", tennis[i+1,119]))
      tbl1 = append(tbl1, grep("7-6", tennis[i+1,120]))
      tbl1 = append(tbl1, grep("13-12", tennis[i+1,120]))
      tbw1 = append(tbw1, grep("6-7", tennis[i+1,116]))
      tbw1 = append(tbw1, grep("6-7", tennis[i+1,117]))
      tbw1 = append(tbw1, grep("6-7", tennis[i+1,118]))
      tbw1 = append(tbw1, grep("6-7", tennis[i+1,119]))
      tbw1 = append(tbw1, grep("6-7", tennis[i+1,120]))
      tbw1 = append(tbw1, grep("12-13", tennis[i+1,120]))
    }
  }

  
  if (length(tbw) + length(tbl) > 0) {
    tennis[j, 112] = length(tbw)/(length(tbw) + length(tbl))
    tennis[j, 114] = length(tbw) + length(tbl)
  } else{
    tennis[j, 112] = 0
    tennis[j, 114] = 0
  }
  
  if (length(tbw1) + length(tbl1) > 0) {
    tennis[j, 113] = length(tbw1)/(length(tbw1) + length(tbl1))
    tennis[j, 115] = length(tbw1) + length(tbl1)
  } else{
    tennis[j, 113] = 0
    tennis[j, 115] = 0
  }
  
  
  if (games > 0){
    tennis[j,38] = servepc/games
    tennis[j,39] = returnpc/games
    tennis[j,40] = sgameswpc/games
    tennis[j,41] = rgameswpc/games
    tennis[j,97] = gamesw/games
    tennis[j,102] = games
  } else{
    tennis[j,38] = 0
    tennis[j,39] = 0
    tennis[j,40] = 0
    tennis[j,41] = 0
    tennis[j,97] = 0
    tennis[j,102] = 0
  }
  
  if (games1 > 0){
    tennis[j,70] = servepc1/games1
    tennis[j,71] = returnpc1/games1
    tennis[j,72] = sgameswpc1/games1
    tennis[j,73] = rgameswpc1/games1
    tennis[j,99] = gamesw1/games1
    tennis[j,103] = games1
  } else{
    tennis[j,70] = 0
    tennis[j,71] = 0
    tennis[j,72] = 0
    tennis[j,73] = 0
    tennis[j,99] = 0
    tennis[j,103] = 0
  }
}


# to calculate a range of average back statistics against all players and on same surface 

for (j in 1:7607){
  
  games = 0
  gamesw = 0
  servepc = 0
  returnpc = 0
  sgameswpc = 0
  rgameswpc = 0
  
  games1 = 0
  gamesw1 = 0
  servepc1 = 0
  returnpc1 = 0
  sgameswpc1 = 0
  rgameswpc1 = 0
  
  
  for (i in j:(j+5081)){
    
    if ((tennis[i+1,10] == tennis[j,10]) & (tennis[i+1,4] == tennis[j,4])){
      servepc = servepc + as.numeric(paste(tennis[i+1,33]))
      returnpc = returnpc + as.numeric(paste(tennis[i+1,34]))
      sgameswpc = sgameswpc + as.numeric(paste(tennis[i+1,35]))
      rgameswpc = rgameswpc + as.numeric(paste(tennis[i+1,36]))
      games = games + 1
      gamesw = gamesw + 1
    } 
    
    if ((tennis[i+1,14] == tennis[j,10]) & (tennis[i+1,4] == tennis[j,4])){
      servepc = servepc + as.numeric(paste(tennis[i+1,65]))
      returnpc = returnpc + as.numeric(paste(tennis[i+1,66]))
      sgameswpc = sgameswpc + as.numeric(paste(tennis[i+1,67]))
      rgameswpc = rgameswpc + as.numeric(paste(tennis[i+1,68]))
      games = games + 1
    }
    
    if ((tennis[i+1,10] == tennis[j,14]) & (tennis[i+1,4] == tennis[j,4])){
      servepc1 = servepc1 + as.numeric(paste(tennis[i+1,33]))
      returnpc1 = returnpc1 + as.numeric(paste(tennis[i+1,34]))
      sgameswpc1 = sgameswpc1 + as.numeric(paste(tennis[i+1,35]))
      rgameswpc1 = rgameswpc1 + as.numeric(paste(tennis[i+1,36]))
      games1 = games1 + 1
      gamesw1 = gamesw1 + 1
    }
    
    if ((tennis[i+1,14] == tennis[j,14]) & (tennis[i+1,4] == tennis[j,4])){
      servepc1 = servepc1 + as.numeric(paste(tennis[i+1,65]))
      returnpc1 = returnpc1 + as.numeric(paste(tennis[i+1,66]))
      sgameswpc1 = sgameswpc1 + as.numeric(paste(tennis[i+1,67]))
      rgameswpc1 = rgameswpc1 + as.numeric(paste(tennis[i+1,68]))
      games1 = games1 + 1
    }
  }
  
  
  if (games > 0){
    tennis[j,43] = servepc/games
    tennis[j,44] = returnpc/games
    tennis[j,45] = sgameswpc/games
    tennis[j,46] = rgameswpc/games
    tennis[j,98] = gamesw/games
    tennis[j,104] = games
  } else{
    tennis[j,43] = 0
    tennis[j,44] = 0
    tennis[j,45] = 0
    tennis[j,46] = 0
    tennis[j,98] = 0
    tennis[j,104] = 0
  }
  
  if (games1 > 0){
    tennis[j,75] = servepc1/games1
    tennis[j,76] = returnpc1/games1
    tennis[j,77] = sgameswpc1/games1
    tennis[j,78] = rgameswpc1/games1
    tennis[j,100] = gamesw1/games1
    tennis[j,105] = games1
  } else{
    tennis[j,75] = 0
    tennis[j,76] = 0
    tennis[j,77] = 0
    tennis[j,78] = 0
    tennis[j,100] = 0
    tennis[j,105] = 0
  }
}



# to calculate head to head numbers and head to head statistics on all surfaces 

for (j in 1:7607){
  
  hthw = 0
  hthl = 0
  games = 0
  servepc = 0
  returnpc = 0
  sgameswpc = 0
  rgameswpc = 0
  
  servepc1 = 0
  returnpc1 = 0
  sgameswpc1 = 0
  rgameswpc1 = 0
  
  
  for (i in j:(j+7544)){
    
    if ((tennis[i+1,10] == tennis[j,10]) & (tennis[i+1,14] == tennis[j,14])){
      servepc = servepc + as.numeric(paste(tennis[i+1,33]))
      returnpc = returnpc + as.numeric(paste(tennis[i+1,34]))
      sgameswpc = sgameswpc + as.numeric(paste(tennis[i+1,35]))
      rgameswpc = rgameswpc + as.numeric(paste(tennis[i+1,36]))
      games = games + 1
      hthw = hthw + 1
      
      servepc1 = servepc1 + as.numeric(paste(tennis[i+1,65]))
      returnpc1 = returnpc1 + as.numeric(paste(tennis[i+1,66]))
      sgameswpc1 = sgameswpc1 + as.numeric(paste(tennis[i+1,67]))
      rgameswpc1 = rgameswpc1 + as.numeric(paste(tennis[i+1,68]))
    }
    
    if ((tennis[i+1,14] == tennis[j,10]) & (tennis[i+1,10] == tennis[j,14])){
      servepc = servepc + as.numeric(paste(tennis[i+1,65]))
      returnpc = returnpc + as.numeric(paste(tennis[i+1,66]))
      sgameswpc = sgameswpc + as.numeric(paste(tennis[i+1,67]))
      rgameswpc = rgameswpc + as.numeric(paste(tennis[i+1,68]))
      games = games + 1
      hthl = hthl + 1
      
      servepc1 = servepc1 + as.numeric(paste(tennis[i+1,33]))
      returnpc1 = returnpc1 + as.numeric(paste(tennis[i+1,34]))
      sgameswpc1 = sgameswpc1 + as.numeric(paste(tennis[i+1,35]))
      rgameswpc1 = rgameswpc1 + as.numeric(paste(tennis[i+1,36]))
    }
  }
  
  if (games > 0){
    tennis[j,48] = servepc/games
    tennis[j,49] = returnpc/games
    tennis[j,50] = sgameswpc/games
    tennis[j,51] = rgameswpc/games
    tennis[j,93] = hthw/games
    tennis[j,94] = hthl/games
    tennis[j,106] = games
  } else{
    tennis[j,48] = 0
    tennis[j,49] = 0
    tennis[j,50] = 0
    tennis[j,51] = 0
    tennis[j,93] = 0
    tennis[j,94] = 0
    tennis[j,106] = 0
  }
  
  if (games > 0){
    tennis[j,80] = servepc1/games
    tennis[j,81] = returnpc1/games
    tennis[j,82] = sgameswpc1/games
    tennis[j,83] = rgameswpc1/games
  } else{
    tennis[j,80] = 0
    tennis[j,81] = 0
    tennis[j,82] = 0
    tennis[j,83] = 0
  }

}


# to calculate head to head numbers and head to head statistics on same surface


for (j in 1:7607){
  
  hthwos = 0
  hthlos = 0
  games = 0
  servepc = 0
  returnpc = 0
  sgameswpc = 0
  rgameswpc = 0
  
  servepc1 = 0
  returnpc1 = 0
  sgameswpc1 = 0
  rgameswpc1 = 0
  
  
  for (i in j:(j+7544)){
    
    if ((tennis[i+1,10] == tennis[j,10]) & (tennis[i+1,14] == tennis[j,14]) & (tennis[i+1,4] == tennis[j,4])){
      servepc = servepc + as.numeric(paste(tennis[i+1,33]))
      returnpc = returnpc + as.numeric(paste(tennis[i+1,34]))
      sgameswpc = sgameswpc + as.numeric(paste(tennis[i+1,35]))
      rgameswpc = rgameswpc + as.numeric(paste(tennis[i+1,36]))
      games = games + 1
      hthwos = hthwos + 1
      
      servepc1 = servepc1 + as.numeric(paste(tennis[i+1,65]))
      returnpc1 = returnpc1 + as.numeric(paste(tennis[i+1,66]))
      sgameswpc1 = sgameswpc1 + as.numeric(paste(tennis[i+1,67]))
      rgameswpc1 = rgameswpc1 + as.numeric(paste(tennis[i+1,68]))
    }
    
    if ((tennis[i+1,14] == tennis[j,10]) & (tennis[i+1,10] == tennis[j,14]) & (tennis[i+1,4] == tennis[j,4])){
      servepc = servepc + as.numeric(paste(tennis[i+1,65]))
      returnpc = returnpc + as.numeric(paste(tennis[i+1,66]))
      sgameswpc = sgameswpc + as.numeric(paste(tennis[i+1,67]))
      rgameswpc = rgameswpc + as.numeric(paste(tennis[i+1,68]))
      games = games + 1
      hthlos = hthlos + 1
      
      servepc1 = servepc1 + as.numeric(paste(tennis[i+1,33]))
      returnpc1 = returnpc1 + as.numeric(paste(tennis[i+1,34]))
      sgameswpc1 = sgameswpc1 + as.numeric(paste(tennis[i+1,35]))
      rgameswpc1 = rgameswpc1 + as.numeric(paste(tennis[i+1,36]))
    }
  }
  
  
  if (games > 0){
    tennis[j,53] = servepc/games
    tennis[j,54] = returnpc/games
    tennis[j,55] = sgameswpc/games
    tennis[j,56] = rgameswpc/games
    tennis[j,95] = hthwos/games
    tennis[j,96] = hthlos/games
    tennis[j,107] = games
  } else{
    tennis[j,53] = 0
    tennis[j,54] = 0
    tennis[j,55] = 0
    tennis[j,56] = 0
    tennis[j,95] = 0
    tennis[j,96] = 0
    tennis[j,107] = 0
  }
  
  if (games > 0){
    tennis[j,85] = servepc1/games
    tennis[j,86] = returnpc1/games
    tennis[j,87] = sgameswpc1/games
    tennis[j,88] = rgameswpc1/games
  } else{
    tennis[j,85] = 0
    tennis[j,86] = 0
    tennis[j,87] = 0
    tennis[j,88] = 0
  }
  
}



# to calculate average statistics for matches between common opponents


for (j in 1:7607){

  wo = c()
  lo = c()
  
  for (i in j:(j+5081)){
    
    if (tennis[i+1,10] == tennis[j,10]){
      if(is.element(tennis[i+1,14], wo)) {
        wo = wo
      } else {
        wo=append(wo, tennis[i+1,14])
      }  
    }
    
    if (tennis[i+1,14] == tennis[j,10]){
      if(is.element(tennis[i+1,10], wo)) {
        wo = wo
      } else {
        wo=append(wo, tennis[i+1,10])
      } 
    }
    if (tennis[i+1,10] == tennis[j,14]){
      if(is.element(tennis[i+1,14], lo)) {
        lo = lo
      } else {
        lo=append(lo, tennis[i+1,14])
      }  
    }
    
    if (tennis[i+1,14] == tennis[j,14]){
      if(is.element(tennis[i+1,10], lo)) {
        lo = lo
      } else {
        lo=append(lo, tennis[i+1,10])
      } 
    }  
  }      
  
  
  
  co = c()
  
  
  if (length(wo) > 0){
    temp = length(wo)
  } else {
    temp = 1
    wo = c(0)
  }
  
  
  for (i in 1:temp){
    if(is.element(wo[i], lo)){
      co = append(co, wo[i])
    }
  }
  
  
  if (length(co) > 0){
  
    a = c()
    b = c()
    c=  c()
    d = c()
    g = c()
    h = c()
    u = c()
    v = c()
    k = c()
    l = c()
    m = c()
    n = c()
    o = c()
    p = c()
    q = c()
    r = c()
    gp = c()
    gw = c()
    gp1 = c()
    gw1 = c()


  
    for (i in 1:length(co)){
      a[i] = 0
      b[i] = 0
      c[i] = 0
      d[i] = 0
      g[i] = 0
      h[i] = 0
      u[i] = 0
      v[i] = 0
      k[i] = 0
      l[i] = 0
      m[i] = 0
      n[i] = 0
      o[i] = 0
      p[i] = 0
      q[i] = 0
      r[i] = 0
      gp[i] = 0
      gw[i] = 0
      gp1[i] = 0
      gw1[i] = 0
    }
    
  
  
    games = 0
    games1 =0
    
  
  
    
    for (i in j:(j+5081)){
      
      if ((tennis[i+1,10] == tennis[j,10]) & (is.element(tennis[i+1,14],co))){
        for (s in 1:length(co)){
          if (tennis[i+1,14] == co[s]){
            a[s] = a[s] + 1
            b[s] = b[s] + as.numeric(paste(tennis[i+1,33]))
            g[s] = g[s] + 1
            h[s] = h[s] + as.numeric(paste(tennis[i+1,34]))
            k[s] = k[s] + 1
            l[s] = l[s] + as.numeric(paste(tennis[i+1,35]))
            o[s] = o[s] + 1
            p[s] = p[s] + as.numeric(paste(tennis[i+1,36]))
            gp[s] = gp[s] +1
            gw[s] = gw[s] + 1
          }
        }
        games = games + 1
      }
      
      if ((tennis[i+1,14] == tennis[j,10]) & (is.element(tennis[i+1,10],co))){
        for (s in 1:length(co)){
          if (tennis[i+1,10] == co[s]){
            a[s] = a[s] + 1
            b[s] = b[s] + as.numeric(paste(tennis[i+1,65]))
            g[s] = g[s] + 1
            h[s] = h[s] + as.numeric(paste(tennis[i+1,66]))
            k[s] = k[s] + 1
            l[s] = l[s] + as.numeric(paste(tennis[i+1,67]))
            o[s] = o[s] + 1
            p[s] = p[s] + as.numeric(paste(tennis[i+1,68]))
            gp[s] = gp[s] +1
          }
        }
        games = games + 1
      }
      
      
      if ((tennis[i+1,10] == tennis[j,14]) & (is.element(tennis[i+1,14],co))){
        for (s in 1:length(co)){
          if (tennis[i+1,14] == co[s]){
            c[s] = c[s] + 1
            d[s] = d[s] + as.numeric(paste(tennis[i+1,33]))
            u[s] = u[s] + 1
            v[s] = v[s] + as.numeric(paste(tennis[i+1,34]))
            m[s] = m[s] + 1
            n[s] = n[s] + as.numeric(paste(tennis[i+1,35]))
            q[s] = q[s] + 1
            r[s] = r[s] + as.numeric(paste(tennis[i+1,36]))
            gp1[s] = gp1[s] +1
            gw1[s] = gw1[s] + 1
          }
        }
        games1 = games1 + 1
      }
      
      if ((tennis[i+1,14] == tennis[j,14]) & (is.element(tennis[i+1,10],co))){
        for (s in 1:length(co)){
          if (tennis[i+1,10] == co[s]){
            c[s] = c[s] + 1
            d[s] = d[s] + as.numeric(paste(tennis[i+1,65]))
            u[s] = u[s] + 1
            v[s] = v[s] + as.numeric(paste(tennis[i+1,66]))
            m[s] = m[s] + 1
            n[s] = n[s] + as.numeric(paste(tennis[i+1,67]))
            q[s] = q[s] + 1
            r[s] = r[s] + as.numeric(paste(tennis[i+1,68]))
            gp1[s] = gp1[s] +1
          }
        }
        games1 = games1 + 1
      }
      
    }
  
    servepc = 0
    returnpc = 0
    sgameswpc = 0
    rgameswpc = 0
    gwpc = 0
    
    
    servepc1 = 0
    returnpc1 = 0
    sgameswpc1 = 0
    rgameswpc1 = 0
    gwpc1 = 0
  
  
  
    for (f in 1:length(co)){
      servepc = servepc + (b[f]/a[f])
      servepc1 = servepc1 + (d[f]/c[f])
      returnpc = returnpc + (h[f]/g[f])
      returnpc1 = returnpc1 + (v[f]/u[f])
      sgameswpc = sgameswpc + (l[f]/k[f])
      sgameswpc1 = sgameswpc1 + (n[f]/m[f])
      rgameswpc = rgameswpc + (p[f]/o[f])
      rgameswpc1 =rgameswpc1 + (r[f]/q[f])
      gwpc = gwpc + (gw[f]/gp[f])
      gwpc1 = gwpc1 + (gw1[f]/gp1[f])
    }
  
  
  
  
    tennis[j, 108] = (servepc/length(co))
    tennis[j, 109] = (returnpc/length(co))
    tennis[j, 110] = (sgameswpc/length(co))
    tennis[j, 111] = (rgameswpc/length(co))
    tennis[j, 121] = (servepc1/length(co))
    tennis[j, 122] = (returnpc1/length(co))
    tennis[j, 123] = (sgameswpc1/length(co))
    tennis[j, 124] = (rgameswpc1/length(co))
    tennis[j, 125] = (gwpc/length(co))
    tennis[j, 126] = (gwpc1/length(co))
    tennis[j, 127] = games
    tennis[j, 128] = games1

  } else{
    tennis[j, 108] = 0
    tennis[j, 109] = 0
    tennis[j, 110] = 0
    tennis[j, 111] = 0
    tennis[j, 121] = 0
    tennis[j, 122] = 0
    tennis[j, 123] = 0
    tennis[j, 124] = 0
    tennis[j, 125] = 0
    tennis[j, 126] = 0
    tennis[j, 127] = 0
    tennis[j, 128] = 0
  }
  
}



# to calculate average statistics for matches between common opponents on same surface

for (j in 1:7670){
  
  wo = c()
  lo = c()
  
  for (i in j:(j+5081)){
    
    if ((tennis[i+1,10] == tennis[j,10])& (tennis[i+1,4] == tennis[j,4])){
      if(is.element(tennis[i+1,14], wo)) {
        wo = wo
      } else {
        wo=append(wo, tennis[i+1,14])
      }  
    }
    
    if ((tennis[i+1,14] == tennis[j,10]) & (tennis[i+1,4] == tennis[j,4])){
      if(is.element(tennis[i+1,10], wo)) {
        wo = wo
      } else {
        wo=append(wo, tennis[i+1,10])
      } 
    }
    if ((tennis[i+1,10] == tennis[j,14]) & (tennis[i+1,4] == tennis[j,4])){
      if(is.element(tennis[i+1,14], lo)) {
        lo = lo
      } else {
        lo=append(lo, tennis[i+1,14])
      }  
    }
    
    if ((tennis[i+1,14] == tennis[j,14]) & (tennis[i+1,4] == tennis[j,4])){
      if(is.element(tennis[i+1,10], lo)) {
        lo = lo
      } else {
        lo=append(lo, tennis[i+1,10])
      } 
    }  
  }      
  
  
  
  co = c()
  
  if (length(wo) > 0){
    temp = length(wo)
  } else {
    temp = 1
    wo = c(0)
  }
  
  
  for (i in 1:temp){
    if(is.element(wo[i], lo)){
      co = append(co, wo[i])
    }
  }

    
  if (length(co) > 0){
  
    a = c()
    b = c()
    c=  c()
    d = c()
    g = c()
    h = c()
    u = c()
    v = c()
    k = c()
    l = c()
    m = c()
    n = c()
    o = c()
    p = c()
    q = c()
    r = c()
    gp = c()
    gw = c()
    gp1 = c()
    gw1 = c()
    
  
  
    for (i in 1:length(co)){
      a[i] = 0
      b[i] = 0
      c[i] = 0
      d[i] = 0
      g[i] = 0
      h[i] = 0
      u[i] = 0
      v[i] = 0
      k[i] = 0
      l[i] = 0
      m[i] = 0
      n[i] = 0
      o[i] = 0
      p[i] = 0
      q[i] = 0
      r[i] = 0
      gp[i] = 0
      gw[i] = 0
      gp1[i] = 0
      gw1[i] = 0
    }
    
    
    
    games = 0
    games1 =0
    
  
    for (i in j:(j+5081)){
      
      if ((tennis[i+1,10] == tennis[j,10]) & (is.element(tennis[i+1,14],co))& (tennis[i+1,4] == tennis[j,4])){
        for (s in 1:length(co)){
          if (tennis[i+1,14] == co[s]){
            a[s] = a[s] + 1
            b[s] = b[s] + as.numeric(paste(tennis[i+1,33]))
            g[s] = g[s] + 1
            h[s] = h[s] + as.numeric(paste(tennis[i+1,34]))
            k[s] = k[s] + 1
            l[s] = l[s] + as.numeric(paste(tennis[i+1,35]))
            o[s] = o[s] + 1
            p[s] = p[s] + as.numeric(paste(tennis[i+1,36]))
            gp[s] = gp[s] +1
            gw[s] = gw[s] + 1
          }
        }
        games = games + 1
      }
      
      if ((tennis[i+1,14] == tennis[j,10]) & (is.element(tennis[i+1,10],co)) & (tennis[i+1,4] == tennis[j,4])){
        for (s in 1:length(co)){
          if (tennis[i+1,10] == co[s]){
            a[s] = a[s] + 1
            b[s] = b[s] + as.numeric(paste(tennis[i+1,65]))
            g[s] = g[s] + 1
            h[s] = h[s] + as.numeric(paste(tennis[i+1,66]))
            k[s] = k[s] + 1
            l[s] = l[s] + as.numeric(paste(tennis[i+1,67]))
            o[s] = o[s] + 1
            p[s] = p[s] + as.numeric(paste(tennis[i+1,68]))
            gp[s] = gp[s] +1
          }
        }
        games = games + 1
      }
      
      
      if ((tennis[i+1,10] == tennis[j,14]) & (is.element(tennis[i+1,14],co)) & (tennis[i+1,4] == tennis[j,4])){
        for (s in 1:length(co)){
          if (tennis[i+1,14] == co[s]){
            c[s] = c[s] + 1
            d[s] = d[s] + as.numeric(paste(tennis[i+1,33]))
            u[s] = u[s] + 1
            v[s] = v[s] + as.numeric(paste(tennis[i+1,34]))
            m[s] = m[s] + 1
            n[s] = n[s] + as.numeric(paste(tennis[i+1,35]))
            q[s] = q[s] + 1
            r[s] = r[s] + as.numeric(paste(tennis[i+1,36]))
            gp1[s] = gp1[s] +1
            gw1[s] = gw1[s] + 1
          }
        }
        games1 = games1 + 1
      }
      
      if ((tennis[i+1,14] == tennis[j,14]) & (is.element(tennis[i+1,10],co)) & (tennis[i+1,4] == tennis[j,4])){
        for (s in 1:length(co)){
          if (tennis[i+1,10] == co[s]){
            c[s] = c[s] + 1
            d[s] = d[s] + as.numeric(paste(tennis[i+1,65]))
            u[s] = u[s] + 1
            v[s] = v[s] + as.numeric(paste(tennis[i+1,66]))
            m[s] = m[s] + 1
            n[s] = n[s] + as.numeric(paste(tennis[i+1,67]))
            q[s] = q[s] + 1
            r[s] = r[s] + as.numeric(paste(tennis[i+1,68]))
            gp1[s] = gp1[s] +1
          }
        }
        games1 = games1 + 1
      }
      
    }
    
    servepc = 0
    returnpc = 0
    sgameswpc = 0
    rgameswpc = 0
    gwpc = 0
    
    
    servepc1 = 0
    returnpc1 = 0
    sgameswpc1 = 0
    rgameswpc1 = 0
    gwpc1 = 0
  
  
  
    for (f in 1:length(co)){
      servepc = servepc + (b[f]/a[f])
      servepc1 = servepc1 + (d[f]/c[f])
      returnpc = returnpc + (h[f]/g[f])
      returnpc1 = returnpc1 + (v[f]/u[f])
      sgameswpc = sgameswpc + (l[f]/k[f])
      sgameswpc1 = sgameswpc1 + (n[f]/m[f])
      rgameswpc = rgameswpc + (p[f]/o[f])
      rgameswpc1 =rgameswpc1 + (r[f]/q[f])
      gwpc = gwpc + (gw[f]/gp[f])
      gwpc1 = gwpc1 + (gw1[f]/gp1[f])
    }
  
  
    tennis[j, 129] = (servepc/length(co))
    tennis[j, 130] = (returnpc/length(co))
    tennis[j, 131] = (sgameswpc/length(co))
    tennis[j, 132] = (rgameswpc/length(co))
    tennis[j, 133] = (servepc1/length(co))
    tennis[j, 134] = (returnpc1/length(co))
    tennis[j, 135] = (sgameswpc1/length(co))
    tennis[j, 136] = (rgameswpc1/length(co))
    tennis[j, 137] = (gwpc/length(co))
    tennis[j, 138] = (gwpc1/length(co))
    tennis[j, 139] = games
    tennis[j, 140] = games1

  } else{
    tennis[j, 129] = 0
    tennis[j, 130] = 0
    tennis[j, 131] = 0
    tennis[j, 132] = 0
    tennis[j, 133] = 0
    tennis[j, 134] = 0
    tennis[j, 135] = 0
    tennis[j, 136] = 0
    tennis[j, 137] = 0
    tennis[j, 138] = 0
    tennis[j, 139] = 0
    tennis[j, 140] = 0
  }
  
}



  
# to calculate if there is home advantage for referenced player

for (i in 1:nrow(tennis)){
  tennis[i,2] = 0
  if (tennis[i,1] == tennis[i,12]){
    if ((i%%2) == 0){
      tennis[i,2] = tennis[i,2] -1
    } else{
      tennis[i,2] = tennis[i,2] +1
    }
  }
  if (tennis[i,1] == tennis[i,16]){
    if ((i%%2) == 0){
      tennis[i,2] = tennis[i,2] +1
    } else{
      tennis[i,2] = tennis[i,2] -1
    }
  }
}


# to calculate is there is left handed advantage for referenced player

for (i in 1:nrow(tennis)){
  tennis[i,18] = 0
  if ((tennis[i,11] == "L") & (tennis[i,15] != "L")){
    if ((i%%2) == 0){
      tennis[i,18] = tennis[i,18] -1
    } else{
      tennis[i,18] = tennis[i,18] +1
    }
  }
  if ((tennis[i,11] != "L") & (tennis[i,15] == "L")){
    if ((i%%2) == 0){
      tennis[i,18] = tennis[i,18] +1
    } else{
      tennis[i,18] = tennis[i,18] -1
    }
  }
}


# to calculate the player combined statistics required for 'referenced player' anlaysis

for (i in 1:7607){
  tennis[i,143] = tennis[i,2]
}


for (i in 1:7607){
  tennis[i,144] = tennis[i,5]
}

for (i in 1:7607){
  tennis[i,145] = tennis[i,6]
}

for (i in 1:7607){
  tennis[i,146] = tennis[i,7]
}

for (i in 1:7607){
  tennis[i,147] = tennis[i,9]
}

for (i in 1:7607){
  tennis[i,148] = tennis[i,18]
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,149] = tennis[i,13] - tennis[i,17]
  } else {
    tennis[i,149] = tennis[i,17] - tennis[i,13]
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,150] = tennis[i,23] - tennis[i,24]
  } else {
    tennis[i,150] = tennis[i,24] - tennis[i,23]
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,151] = tennis[i,90] - tennis[i,92]
  } else {
    tennis[i,151] = tennis[i,92] - tennis[i,90]
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,152] = ((tennis[i,38])/(tennis[i,38] + tennis[i,71])) 
  } else {
    tennis[i,152] = ((tennis[i,70])/(tennis[i,39] + tennis[i,70])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,153] = ((tennis[i,39])/(tennis[i,39] + tennis[i,70])) 
  } else {
    tennis[i,153] = ((tennis[i,71])/(tennis[i,38] + tennis[i,71])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,154] = ((tennis[i,40])/(tennis[i,40] + tennis[i,73])) 
  } else {
    tennis[i,154] = ((tennis[i,72])/(tennis[i,41] + tennis[i,72])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,155] = ((tennis[i,41])/(tennis[i,41] + tennis[i,72])) 
  } else {
    tennis[i,155] = ((tennis[i,73])/(tennis[i,40] + tennis[i,73])) 
  }
  
}



for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,156] = ((tennis[i,43])/(tennis[i,43] + tennis[i,76])) 
  } else {
    tennis[i,156] = ((tennis[i,75])/(tennis[i,44] + tennis[i,75])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,157] = ((tennis[i,44])/(tennis[i,44] + tennis[i,75]))
  } else {
    tennis[i,157] = ((tennis[i,76])/(tennis[i,43] + tennis[i,76])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,158] = ((tennis[i,45])/(tennis[i,45] + tennis[i,78]))
  } else {
    tennis[i,158] = ((tennis[i,77])/(tennis[i,46] + tennis[i,77])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,159] = ((tennis[i,46])/(tennis[i,46] + tennis[i,77]))
  } else {
    tennis[i,159] = ((tennis[i,78])/(tennis[i,45] + tennis[i,78]))
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,160] = ((tennis[i,48])/(tennis[i,48] + tennis[i,81])) 
  } else {
    tennis[i,160] = ((tennis[i,80])/(tennis[i,49] + tennis[i,80])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,161] = ((tennis[i,49])/(tennis[i,49] + tennis[i,80])) 
  } else {
    tennis[i,161] = ((tennis[i,81])/(tennis[i,48] + tennis[i,81])) 
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,162] = ((tennis[i,50])/(tennis[i,50] + tennis[i,83])) 
  } else {
    tennis[i,162] = ((tennis[i,82])/(tennis[i,51] + tennis[i,82])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,163] = ((tennis[i,51])/(tennis[i,51] + tennis[i,82])) 
  } else {
    tennis[i,163] = ((tennis[i,83])/(tennis[i,50] + tennis[i,83])) 
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,164] = ((tennis[i,53])/(tennis[i,53] + tennis[i,86])) 
  } else {
    tennis[i,164] = ((tennis[i,85])/(tennis[i,54] + tennis[i,85])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,165] = ((tennis[i,54])/(tennis[i,54] + tennis[i,85])) 
  } else {
    tennis[i,165] = ((tennis[i,86])/(tennis[i,53] + tennis[i,86])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,166] = ((tennis[i,55])/(tennis[i,55] + tennis[i,88])) 
  } else {
    tennis[i,166] = ((tennis[i,87])/(tennis[i,56] + tennis[i,87]))
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,167] = ((tennis[i,56])/(tennis[i,56] + tennis[i,87])) 
  } else {
    tennis[i,167] = ((tennis[i,88])/(tennis[i,55] + tennis[i,88])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,168] = ((tennis[i,108])/(tennis[i,108] + tennis[i,122])) 
  } else {
    tennis[i,168] = ((tennis[i,121])/(tennis[i,109] + tennis[i,121])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,169] = ((tennis[i,109])/(tennis[i,109] + tennis[i,121]))
  } else {
    tennis[i,169] = ((tennis[i,122])/(tennis[i,108] + tennis[i,122])) 
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,170] = ((tennis[i,110])/(tennis[i,110] + tennis[i,124]))
  } else {
    tennis[i,170] = ((tennis[i,123])/(tennis[i,111] + tennis[i,123])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,171] = ((tennis[i,111])/(tennis[i,111] + tennis[i,123])) 
  } else {
    tennis[i,171] = ((tennis[i,124])/(tennis[i,110] + tennis[i,124])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,172] = ((tennis[i,129])/(tennis[i,129] + tennis[i,134])) 
  } else {
    tennis[i,172] = ((tennis[i,133])/(tennis[i,130] + tennis[i,133])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,173] = ((tennis[i,130])/(tennis[i,130] + tennis[i,133])) 
  } else {
    tennis[i,173] = ((tennis[i,134])/(tennis[i,129] + tennis[i,134])) 
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,174] = ((tennis[i,131])/(tennis[i,131] + tennis[i,136])) 
  } else {
    tennis[i,174] = ((tennis[i,135])/(tennis[i,132] + tennis[i,135])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,175] = ((tennis[i,132])/(tennis[i,132] + tennis[i,135])) 
  } else {
    tennis[i,175] = ((tennis[i,136])/(tennis[i,131] + tennis[i,136])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,176] = ((tennis[i,97])/(tennis[i,97] + tennis[i,99])) 
  } else {
    tennis[i,176] = ((tennis[i,99])/(tennis[i,97] + tennis[i,99])) 
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,177] = ((tennis[i,98])/(tennis[i,98] + tennis[i,100])) 
  } else {
    tennis[i,177] = ((tennis[i,100])/(tennis[i,98] + tennis[i,100])) 
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,178] = ((tennis[i,93])/(tennis[i,93] + tennis[i,94])) 
  } else {
    tennis[i,178] = ((tennis[i,94])/(tennis[i,93] + tennis[i,94])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,179] = ((tennis[i,95])/(tennis[i,95] + tennis[i,96])) 
  } else {
    tennis[i,179] = ((tennis[i,96])/(tennis[i,95] + tennis[i,96])) 
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,180] = ((tennis[i,125])/(tennis[i,125] + tennis[i,126])) 
  } else {
    tennis[i,180] = ((tennis[i,126])/(tennis[i,125] + tennis[i,126])) 
  }
  
}

for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,181] = ((tennis[i,137])/(tennis[i,137] + tennis[i,138])) 
  } else {
    tennis[i,181] = ((tennis[i,138])/(tennis[i,137] + tennis[i,138])) 
  }
  
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,182] = ((tennis[i,112])/(tennis[i,112] + tennis[i,113])) 
  } else {
    tennis[i,182] = ((tennis[i,113])/(tennis[i,112] + tennis[i,113])) 
  }
  
}

for (i in 1:7607){
  if (tennis[i,101] == "W"){
    tennis[i,183] = tennis[i,141]
  } else{
    tennis[i,183] = tennis[i,142]
  }
}



for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,184] = "W"
  } else{
    tennis[i,184] = "L"
  }
}


for (i in 1:7607){
  if (tennis[i,184] == "W"){
    tennis[i,185] = 1
  } else{
    tennis[i,185] = 0
  }
}

for(i in 1:7607){
  tennis[i,186] = tennis[i,4]
}


for (i in 1:7607){
  if(tennis[i,101] == "W"){
    tennis[i,187] = tennis[i,91] - tennis[i,89]
  } else {
    tennis[i,187] = tennis[i,89] - tennis[i,91]
  }
  
}


# to remove cases where denominator was = 0 

for (i in 152:182){
  tennis[,i] = replace(tennis[,i],is.na(tennis[,i]),0)
}


path = "D:\\Project_pt7.csv"
write.csv(tennis, path)


# to create train and test sets of relevant columns for logistic regression and other analysis

tennis1 = tennis

for(i in 1:142){
  tennis1 = tennis1[, -1]
}

tennis1 = tennis1[, -2]
tennis1 = tennis1[, -2]
tennis1 = tennis1[, -2]


tennis1 = tennis1[, -38]
tennis1 = tennis1[, -38]


for (i in 1:7544){
  tennis1 = tennis1[-7608,]
}



sid=sample.int(n=7607, size=2282)
LRtest = tennis1[sid,]
LRtrain = tennis1[-sid,]

path = "D:\\LRtrain.csv"
write.csv(LRtrain, path)

path = "D:\\LRtest.csv"
write.csv(LRtest, path)


## forward feature slection for LR model 

glm1 = glm(AS_win_lose_binary ~ ., family = binomial(link = logit), data = LRtrain)

summary(glm1)

steptest = step(glm1, scope = . ~ .^2, direction = 'forward')

summary(steptest)


# to attach winner and loser betting odds and 'referenced player' odds onto train and test sets for later

for (i in 1:length(rownames(LRtest))){
  LRtest[i,41] = tennis[row.names(LRtest)[i], 141]
}

for (i in 1:length(rownames(LRtrain))){
  LRtrain[i,41] = tennis[row.names(LRtrain)[i], 141]
}

for (i in 1:length(rownames(LRtest))){
  LRtest[i,42] = tennis[row.names(LRtest)[i], 142]
}

for (i in 1:length(rownames(LRtrain))){
  LRtrain[i,42] = tennis[row.names(LRtrain)[i], 142]
}

for (i in 1:length(rownames(LRtest))){
  LRtest[i,43] = tennis[row.names(LRtest)[i], 183]
}

for (i in 1:length(rownames(LRtrain))){
  LRtrain[i,43] = tennis[row.names(LRtrain)[i], 183]
}


# creating full data set versions of training and test set members for below use

tennistr = tennis
tenniste = tennis

for (i in 1:length(rownames(LRtest))){
  tenniste[i, ] = tennis[row.names(LRtest)[i], ]
}

for (i in 1:length(rownames(LRtrain))){
  tennistr[i, ] = tennis[row.names(LRtrain)[i], ]
}


for (i in 1:12870){
  tenniste = tenniste[-2283,]
}

for (i in 1:9827){
  tennistr = tennistr[-5326,]
}


## creating reduced training and test sets with certain minumim values in certain features

minnotrain = tennistr
minnotest = tenniste


j = c()

for(i in 1:length(rownames(LRtrain))){
  if((tennistr[i,127]>24) & (tennistr[i,128]>24) & (tennistr[i,139]>14) & (tennistr[i,140] > 14) & (tennistr[i,102] > 29) & (tennistr[i,103]> 29) & (tennistr[i,104]>19) & (tennistr[i,105]>19)){
    j = append(j, i)
  }
}

length(j)

k = 1


for (i in 1:length(rownames(LRtrain))){
  if((tennistr[i,127]>24) & (tennistr[i,128]>24) & (tennistr[i,139]>14) & (tennistr[i,140] > 14) & (tennistr[i,102] > 29) & (tennistr[i,103]> 29) & (tennistr[i,104]>19) & (tennistr[i,105]>19)){
    minnotrain[k, ] = tennistr[i, ]
    k = k + 1
    
  }
}



for (i in 1:4324){
  minnotrain = minnotrain[-1002,]
}


j = c()

for(i in 1:length(rownames(LRtest))){
  if((tenniste[i,127]>24) & (tenniste[i,128]>24) & (tenniste[i,139]>14) & (tenniste[i,140] > 14) & (tenniste[i,102] > 29) & (tenniste[i,103]> 29) & (tenniste[i,104]>19) & (tenniste[i,105]>19)){
    j = append(j, i)
  }
}

length(j)

k = 1


for (i in 1:length(rownames(LRtest))){
  if((tenniste[i,127]>24) & (tenniste[i,128]>24) & (tenniste[i,139]>14) & (tenniste[i,140] > 14) & (tenniste[i,102] > 29) & (tenniste[i,103]> 29) & (tenniste[i,104]>19) & (tenniste[i,105]>19)){
    minnotest[k, ] = tenniste[i, ]
    k = k + 1
    
  }
}


for (i in 1:1844){
  minnotest = minnotest[-439,]
}

path = "D:\\tennistr.csv"
write.csv(tennistr, path)

path = "D:\\tenniste.csv"
write.csv(tenniste, path)

path = "D:\\minnotrain.csv"
write.csv(minnotrain, path)

path = "D:\\minnotest.csv"
write.csv(minnotest, path)



## do Logistic regression on steptest variables from above taken from LR train

glm2 = glm(AS_win_lose_binary ~ AS_home_advan + AS_tournie_level + 
      AS_left_hand_advan + AS_age + AS_fatigue + AS_rank_points + 
      AS_serve.returnpt + AS_return.servept + AS_serve.returng + 
      AS_return.serveg + AS_serve.returnpt_os + AS_return.servept_os + 
      AS_serve.returng_os + AS_return.serveg_os + AS_serve.returnpt_hth + 
      AS_return.servept_hth + AS_serve.returng_hth + AS_return.serveg_hth + 
      AS_serve.returnpt_hth_os + AS_return.servept_hth_os + AS_serve.returng_hth_os + 
      AS_return.serveg_hth_os + AS_serve.returnpt_co + AS_return.servept_co + 
      AS_serve.returng_co + AS_return.serveg_co + AS_serve.returnpt_co_os + 
      AS_return.servept_co_os + AS_serve.returng_co_os + AS_return.serveg_co_os + 
      AS_matchw + AS_matchw_os + AS_hth_matchw + AS_hth_matchw_os + 
      AS_matchw_co + AS_matchw_co_os + AS_tbw + surface.LR. + AS_rank + 
      AS_serve.returnpt:AS_return.servept_co + AS_tournie_level:AS_return.servept + 
      AS_return.servept:AS_return.serveg_co_os + AS_fatigue:AS_return.serveg_hth_os + 
      AS_left_hand_advan:AS_serve.returng_os + AS_serve.returng_co:AS_rank + 
      AS_serve.returnpt_os:AS_serve.returng_co + AS_serve.returnpt_co:AS_return.servept_co + 
      AS_serve.returng_hth_os:AS_rank + AS_rank_points:AS_serve.returnpt_hth + 
      AS_serve.returng_hth:AS_matchw + AS_left_hand_advan:AS_return.serveg_co + 
      AS_age:AS_serve.returnpt_hth_os + AS_serve.returng:AS_matchw_co + 
      AS_return.servept_hth_os:AS_rank + AS_tournie_level:AS_serve.returnpt + 
      AS_rank_points:AS_return.serveg_hth_os + AS_return.serveg_hth_os:surface.LR. + 
      AS_fatigue:surface.LR. + AS_serve.returnpt_hth:surface.LR. + 
      AS_return.servept_co_os:AS_return.serveg_co_os + AS_return.servept_hth:surface.LR. + 
      AS_return.serveg_co:AS_matchw_co + AS_return.serveg_co:AS_rank + 
      AS_home_advan:AS_matchw_os + AS_home_advan:AS_return.serveg_os + 
      AS_matchw:surface.LR. + AS_serve.returng_co_os:AS_matchw_co_os + 
      AS_fatigue:AS_serve.returng_co + AS_tbw:AS_rank + AS_serve.returnpt_os:AS_return.servept_co_os + 
      AS_home_advan:AS_rank + AS_tournie_level:AS_return.serveg_hth_os + 
      AS_home_advan:AS_return.servept + AS_serve.returng_hth:AS_return.servept_hth_os + 
      AS_serve.returng:AS_return.serveg_co_os + AS_return.serveg_co_os:AS_matchw_co + 
      AS_tournie_level:AS_return.servept_os + AS_matchw:AS_rank + 
      AS_tournie_level:AS_matchw + AS_tournie_level:AS_matchw_os + 
      AS_tournie_level:AS_rank + AS_left_hand_advan:AS_matchw_co_os + 
      AS_serve.returnpt:AS_return.servept_hth + AS_serve.returnpt_os:AS_return.servept_hth_os + 
      AS_serve.returnpt_hth:AS_return.servept_hth + AS_return.serveg:AS_serve.returng_co + 
      AS_return.servept:AS_return.servept_co_os + AS_tournie_level:AS_return.serveg + 
      AS_fatigue:AS_return.servept_co_os + AS_left_hand_advan:AS_serve.returnpt_os + 
      AS_left_hand_advan:AS_return.servept_os + AS_matchw_co_os:AS_tbw + 
      AS_age:AS_rank_points, family = binomial(link = logit), data = LRtrain)

summary(glm2)

### do stepback on least significant features individually until all are significant (or otherwise need to be retained)

## above yields the below

glm3 = glm(AS_win_lose_binary ~ AS_home_advan + AS_tournie_level + 
             AS_left_hand_advan + AS_age + AS_fatigue + AS_rank_points + 
             AS_serve.returnpt + AS_return.servept + AS_serve.returng + 
             AS_return.serveg + AS_serve.returnpt_os + AS_return.servept_os + AS_return.serveg_os + AS_serve.returnpt_hth + 
             AS_return.servept_hth + AS_serve.returng_hth + 
             AS_serve.returnpt_hth_os + AS_return.servept_hth_os + AS_serve.returng_hth_os + 
             AS_return.serveg_hth_os + AS_serve.returnpt_co + AS_return.servept_co + 
             AS_serve.returng_co + AS_return.serveg_co + AS_serve.returnpt_co_os + 
             AS_return.servept_co_os + AS_serve.returng_co_os + AS_return.serveg_co_os + 
             AS_matchw + AS_matchw_os +
             AS_matchw_co + AS_matchw_co_os + AS_tbw + surface.LR. + AS_rank + 
             AS_serve.returnpt:AS_return.servept_co + AS_tournie_level:AS_return.servept + AS_fatigue:AS_return.serveg_hth_os + 
             AS_left_hand_advan:AS_serve.returng_os + AS_serve.returng_co:AS_rank + 
             AS_serve.returnpt_os:AS_serve.returng_co + AS_serve.returnpt_co:AS_return.servept_co + 
             AS_serve.returng_hth_os:AS_rank + AS_rank_points:AS_serve.returnpt_hth + 
             AS_serve.returng_hth:AS_matchw + AS_left_hand_advan:AS_return.serveg_co + 
             AS_age:AS_serve.returnpt_hth_os + 
             AS_return.servept_hth_os:AS_rank + AS_tournie_level:AS_serve.returnpt + 
             AS_rank_points:AS_return.serveg_hth_os  + AS_serve.returnpt_hth:surface.LR. + 
             AS_return.servept_co_os:AS_return.serveg_co_os + AS_return.servept_hth:surface.LR. + 
             AS_return.serveg_co:AS_matchw_co + AS_return.serveg_co:AS_rank + 
             AS_home_advan:AS_matchw_os + AS_home_advan:AS_return.serveg_os + 
             AS_matchw:surface.LR. + AS_serve.returng_co_os:AS_matchw_co_os + 
             AS_fatigue:AS_serve.returng_co + AS_tbw:AS_rank + AS_serve.returnpt_os:AS_return.servept_co_os + 
             AS_serve.returng_hth:AS_return.servept_hth_os + 
             AS_serve.returng:AS_return.serveg_co_os + AS_return.serveg_co_os:AS_matchw_co + AS_matchw:AS_rank + 
             AS_tournie_level:AS_matchw + AS_tournie_level:AS_matchw_os + 
             AS_serve.returnpt:AS_return.servept_hth + AS_serve.returnpt_os:AS_return.servept_hth_os + 
             AS_return.serveg:AS_serve.returng_co + AS_left_hand_advan:AS_serve.returnpt_os + 
             AS_left_hand_advan:AS_return.servept_os, family = binomial(link = logit), data = LRtrain)

summary(glm3)


qchisq(.95,5247)


## to calulcate accuracy and betting returns

## first self validating against training set

pre = predict(glm3, LRtrain, type = "response")


## normal betting if prob >= 50% and taking accouht of poss few zero's for betting odds

acc = 0 
win = 0
bet = 0


for (i in 1:length(pre)){
  if((LRtrain[i, 38])==0){
    if((pre[i] < 0.50) & (LRtrain[i,41] > 0)){
      acc = acc + 1
      win = win + LRtrain[i,41] 
    }
    if (LRtrain[i,41] >0){
      bet = bet + 1
    }
  } else{
    if((pre[i] >= 0.50) & (LRtrain[i,41] > 0)){
      acc = acc + 1
      win = win + LRtrain[i,41]
    }
    if (LRtrain[i,41] >0){
      bet = bet + 1
    }
  }
}


acc = acc/bet

acc

win = win - bet

ret = win/bet

ret


## refraining betting unless high probs

bet = 0
acc = 0 
win = 0

for (i in 1:length(pre)){
  if((LRtrain[i,38])==0){
    if((pre[i] <= 0.20) & (LRtrain[i,41] > 0)){
      acc = acc + 1
      bet = bet + 1
      win = win + LRtrain[i,41] 
    }
    if ((pre[i] >0.80) & (LRtrain[i,41] > 0)){
      bet = bet + 1
    }
  } else{
    if((pre[i] >= 0.80) & (LRtrain[i,41] > 0)){
      acc = acc + 1
      win = win + LRtrain[i,41]
      bet = bet + 1
    } 
    if((pre[i] < 0.20) & (LRtrain[i,41] > 0)){
      bet = bet + 1
    }
  }
}



acc = acc/bet

acc

win = win - bet

ret = win/bet

ret


## to do grid search of above values to find any more optimal thresholds for use in test set run below

a = 0.50
b = 0.50

for (w in 1:45){
  
  bet = 0
  acc = 0 
  win = 0
  
  for (i in 1:length(pre)){
    if((LRtrain[i,38])==0){
      if((pre[i] <= a) & (LRtrain[i,41] > 0)){
        acc = acc + 1
        bet = bet + 1
        win = win + LRtrain[i,41] 
      }
      if ((pre[i] >b) & (LRtrain[i,41] > 0)){
        bet = bet + 1
      }
    } else{
      if((pre[i] >= b) & (LRtrain[i,41] > 0)){
        acc = acc + 1
        win = win + LRtrain[i,41]
        bet = bet + 1
      } 
      if((pre[i] < a) & (LRtrain[i,41] > 0)){
        bet = bet + 1
      }
    }
  }
  
  
  acc = acc/bet
  
  print(acc)
  
  win = win - bet
  
  print(win/bet)
  
  a = a - .01
  b = b + .01
  
}


## now test against test set 

pre = predict(glm3, LRtest, type = "response")


## normal betting if prob >= 50% and taking accouht of poss few zero's for betting odds

acc = 0 
win = 0
bet = 0

for (i in 1:length(pre)){
  if((LRtest[i, 38])==0){
    if((pre[i] < 0.5) & (LRtest[i,41] > 0)){
      acc = acc + 1
      win = win + LRtest[i,41] 
    }
    if (LRtest[i,41] >0){
      bet = bet + 1
    }
  } else{
    if((pre[i] >= 0.5) & (LRtest[i,41] > 0)){
      acc = acc + 1
      win = win + LRtest[i,41]
    }
    if (LRtest[i,41] >0){
      bet = bet + 1
    }
  }
}


acc = acc/bet

acc

win = win - bet

ret = win/bet

ret


## refraining betting unless high probs

bet = 0
acc = 0 
win = 0

for (i in 1:length(pre)){
  if((LRtest[i,38])==0){
    if((pre[i] <= 0.11) & (LRtest[i,41] > 0)){
      acc = acc + 1
      bet = bet + 1
      win = win + LRtest[i,41] 
    }
    if ((pre[i] >0.89) & (LRtest[i,41] > 0)){
      bet = bet + 1
    }
  } else{
    if((pre[i] >= 0.89) & (LRtest[i,41] > 0)){
      acc = acc + 1
      win = win + LRtest[i,41]
      bet = bet + 1
    } 
    if((pre[i] < 0.11) & (LRtest[i,41] > 0)){
      bet = bet + 1
    }
  }
}


acc = acc/bet

acc

win = win - bet

ret = win/bet

ret


## repeat above with filtered training and test sets with certain minmum figures 

## firt create initial feature list for minnotrain

minnotrain1 = minnotrain

View(colnames(minnotrain1))

for(i in 1:142){
  minnotrain1 = minnotrain1[, -1]
}


minnotrain1 = minnotrain1[, -2]
minnotrain1 = minnotrain1[, -2]
minnotrain1 = minnotrain1[, -2]
minnotrain1 = minnotrain1[, -38]
minnotrain1 = minnotrain1[, -38]



glm4 = glm(AS_win_lose_binary ~ ., family = binomial(link = logit), data = minnotrain1)

summary(glm4)

steptest = step(glm4, scope = . ~ .^2, direction = 'forward')

summary(steptest)


## above function yields the below subset of variables and interactions 

glm5 = glm(AS_win_lose_binary ~ AS_home_advan + AS_tournie_level + 
      AS_left_hand_advan + AS_age + AS_fatigue + AS_rank_points + 
      AS_serve.returnpt + AS_return.servept + AS_serve.returng + 
      AS_return.serveg + AS_serve.returnpt_os + AS_return.servept_os + 
      AS_serve.returng_os + AS_return.serveg_os + AS_serve.returnpt_hth + 
      AS_return.servept_hth + AS_serve.returng_hth + AS_return.serveg_hth + 
      AS_serve.returnpt_hth_os + AS_return.servept_hth_os + AS_serve.returng_hth_os + 
      AS_return.serveg_hth_os + AS_serve.returnpt_co + AS_return.servept_co + 
      AS_serve.returng_co + AS_return.serveg_co + AS_serve.returnpt_co_os + 
      AS_return.servept_co_os + AS_serve.returng_co_os + AS_return.serveg_co_os + 
      AS_matchw + AS_matchw_os + AS_hth_matchw + AS_hth_matchw_os + 
      AS_matchw_co + AS_matchw_co_os + AS_tbw + surface.LR. + AS_rank + 
      AS_tournie_level:AS_return.servept_co_os + AS_matchw_co_os:AS_rank + 
      AS_fatigue:AS_serve.returng_co_os + AS_serve.returng_hth:AS_serve.returnpt_co_os + 
      AS_serve.returng_co_os:AS_matchw_co + AS_fatigue:AS_tbw + 
      AS_return.serveg_os:AS_matchw_os + AS_serve.returnpt:AS_serve.returnpt_hth_os + 
      AS_home_advan:surface.LR. + AS_home_advan:AS_tournie_level + 
      AS_return.servept:AS_return.servept_co_os + AS_serve.returng:AS_matchw + 
      AS_return.servept_os:AS_return.serveg_co + AS_age:AS_fatigue + 
      AS_rank_points:AS_return.servept_hth + AS_serve.returnpt_hth:surface.LR. + 
      AS_return.servept_os:AS_hth_matchw_os + AS_matchw_co:AS_tbw + 
      AS_home_advan:AS_return.serveg_os + AS_return.servept_os:AS_tbw + 
      AS_tournie_level:AS_matchw_co + AS_tournie_level:AS_serve.returnpt_os + 
      AS_serve.returng:AS_matchw_os + AS_rank_points:AS_serve.returng_os + 
      AS_rank_points:AS_hth_matchw + AS_serve.returnpt:AS_hth_matchw + 
      AS_return.serveg_co_os:surface.LR. + AS_return.serveg:AS_serve.returnpt_hth_os + 
      AS_rank_points:AS_return.serveg_hth_os + AS_return.servept_co:AS_hth_matchw + 
      AS_return.serveg_hth_os:AS_matchw + AS_return.serveg_os:AS_hth_matchw + 
      AS_return.servept_hth:AS_hth_matchw + AS_return.serveg_hth_os:AS_serve.returnpt_co_os + 
      AS_return.servept_hth:AS_return.serveg_hth + AS_tbw:AS_rank + 
      AS_serve.returnpt_co:AS_return.servept_co + AS_rank_points:AS_return.servept + 
      AS_serve.returnpt_hth:AS_rank + AS_return.serveg:AS_return.serveg_hth + 
      AS_serve.returng_co_os:AS_matchw_co_os + AS_matchw:AS_matchw_os + 
      AS_return.servept_co_os:AS_tbw + AS_fatigue:AS_return.serveg_hth_os + 
      AS_serve.returng_co:AS_hth_matchw_os + AS_return.servept_os:AS_return.serveg_os + 
      AS_return.servept:AS_serve.returnpt_os + AS_return.servept_co:AS_matchw_co + 
      AS_return.serveg_os:AS_serve.returnpt_co + AS_serve.returng:AS_return.serveg + 
      AS_left_hand_advan:AS_serve.returng_hth_os + AS_return.serveg_hth:AS_serve.returng_co_os + 
      AS_return.servept_os:AS_serve.returnpt_co_os + AS_return.servept:AS_serve.returng_co_os + 
      AS_serve.returnpt_co_os:AS_matchw_co_os + AS_hth_matchw_os:AS_matchw_co + 
      AS_return.servept_co:AS_matchw_os + AS_left_hand_advan:AS_age + 
      AS_fatigue:AS_return.servept_co + AS_fatigue:AS_return.serveg_co_os + 
      AS_age:AS_matchw_co_os + AS_age:AS_serve.returng_os + AS_serve.returnpt_os:AS_return.servept_hth + 
      AS_age:AS_serve.returnpt_co + AS_tournie_level:AS_age + AS_serve.returnpt_co_os:AS_serve.returng_co_os + 
      AS_serve.returnpt_os:AS_serve.returng_co + AS_return.serveg_os:AS_serve.returng_co_os + 
      AS_left_hand_advan:AS_serve.returnpt_hth_os + AS_return.servept_os:AS_return.servept_co + 
      AS_matchw_co:surface.LR. + AS_rank_points:surface.LR. + AS_serve.returnpt:AS_serve.returng + 
      AS_serve.returng_os:AS_return.serveg_os + AS_serve.returnpt:AS_return.serveg + 
      AS_return.serveg:AS_serve.returng_co + AS_matchw:surface.LR. + 
      AS_serve.returng_co_os:surface.LR. + AS_return.servept_hth_os:AS_hth_matchw_os + 
      AS_serve.returnpt_co_os:AS_return.servept_co_os + AS_return.servept_co_os:AS_matchw + 
      AS_serve.returng_os:AS_return.serveg_co_os + AS_return.serveg_co_os:AS_matchw + 
      AS_fatigue:AS_return.serveg_os + AS_return.servept_os:AS_matchw_os + 
      AS_serve.returng_os:AS_matchw_co_os + AS_rank_points:AS_matchw_co + 
      AS_return.servept:AS_return.serveg_co_os + AS_matchw_co_os:surface.LR. + 
      AS_age:AS_hth_matchw + AS_home_advan:AS_return.serveg_co + 
      AS_serve.returng_os:AS_serve.returng_hth + AS_serve.returng_os:AS_return.serveg_hth_os + 
      AS_fatigue:AS_return.serveg_hth + AS_fatigue:AS_hth_matchw + 
      AS_tournie_level:AS_return.servept_os + AS_serve.returnpt_hth:AS_serve.returnpt_co + 
      AS_home_advan:AS_return.servept + AS_serve.returng_os:AS_matchw_os + 
      AS_serve.returng:AS_serve.returnpt_os + AS_serve.returnpt:AS_serve.returng_co + 
      AS_hth_matchw:AS_hth_matchw_os + AS_serve.returnpt_hth:AS_hth_matchw + 
      AS_serve.returnpt_hth:AS_return.servept_hth + AS_return.serveg_hth:AS_serve.returnpt_hth_os + 
      AS_return.servept_hth:AS_return.servept_hth_os + AS_matchw_os:AS_rank + 
      AS_matchw_co:AS_rank + AS_return.servept_co:AS_matchw_co_os + 
      AS_serve.returng_os:AS_serve.returng_hth_os + AS_return.servept_co:AS_tbw + 
      AS_left_hand_advan:AS_return.serveg_hth_os + AS_left_hand_advan:AS_hth_matchw_os + 
      AS_serve.returnpt:AS_matchw_os + AS_return.serveg:AS_serve.returnpt_os + 
      AS_return.servept_os:AS_serve.returnpt_co + AS_fatigue:AS_serve.returng_os + 
      AS_fatigue:AS_rank_points + AS_fatigue:AS_return.servept_co_os + 
      AS_age:AS_tbw + AS_tournie_level:AS_left_hand_advan + AS_serve.returng_hth:AS_return.serveg_hth_os + 
      AS_left_hand_advan:AS_serve.returnpt_hth + AS_left_hand_advan:AS_return.servept_hth + 
      AS_left_hand_advan:AS_serve.returng_co + AS_left_hand_advan:AS_serve.returnpt_os + 
      AS_serve.returnpt_os:AS_serve.returnpt_co_os + AS_left_hand_advan:AS_return.serveg_os + 
      AS_serve.returng_hth:AS_serve.returnpt_hth_os + AS_age:AS_matchw + 
      AS_home_advan:AS_return.serveg_co_os + AS_left_hand_advan:AS_matchw_os, 
    family = binomial(link = logit), data = minnotrain)

summary(glm5)

### do stepback on least significant features individually until all are significant (or otherwise need to be retained)

## above yields the below

glm6 = glm(AS_win_lose_binary ~ AS_home_advan + AS_tournie_level + 
             AS_left_hand_advan + AS_age + AS_fatigue + AS_rank_points + 
             AS_serve.returnpt + AS_return.servept + AS_serve.returng + 
             AS_return.serveg + AS_serve.returnpt_os + AS_return.servept_os + 
             AS_serve.returng_os + AS_return.serveg_os + AS_serve.returnpt_hth + 
             AS_return.servept_hth + AS_serve.returng_hth + AS_return.serveg_hth + 
             AS_serve.returnpt_hth_os + AS_return.servept_hth_os + AS_serve.returng_hth_os + 
             AS_return.serveg_hth_os + AS_serve.returnpt_co + AS_return.servept_co + 
             AS_serve.returng_co + AS_return.serveg_co + AS_serve.returnpt_co_os + 
             AS_return.servept_co_os + AS_serve.returng_co_os + AS_return.serveg_co_os + 
             AS_matchw + AS_matchw_os + AS_hth_matchw + AS_hth_matchw_os + 
             AS_matchw_co + AS_matchw_co_os + AS_tbw + surface.LR. + AS_rank + 
             AS_tournie_level:AS_return.servept_co_os +
             AS_serve.returng_hth:AS_serve.returnpt_co_os + 
             AS_serve.returng_co_os:AS_matchw_co + AS_fatigue:AS_tbw + 
             AS_serve.returnpt:AS_serve.returnpt_hth_os + 
             AS_home_advan:surface.LR. + AS_home_advan:AS_tournie_level + 
             AS_return.servept:AS_return.servept_co_os +  
             AS_return.servept_os:AS_return.serveg_co + AS_age:AS_fatigue + 
             AS_serve.returnpt_hth:surface.LR. + 
             AS_return.servept_os:AS_hth_matchw_os + AS_matchw_co:AS_tbw + 
             AS_tournie_level:AS_matchw_co + AS_tournie_level:AS_serve.returnpt_os + 
             AS_rank_points:AS_serve.returng_os + 
             AS_rank_points:AS_hth_matchw + AS_serve.returnpt:AS_hth_matchw + 
             AS_return.serveg:AS_serve.returnpt_hth_os + 
             AS_rank_points:AS_return.serveg_hth_os + AS_return.servept_co:AS_hth_matchw + 
             AS_return.serveg_hth_os:AS_matchw + AS_return.serveg_os:AS_hth_matchw + 
             AS_return.serveg_hth_os:AS_serve.returnpt_co_os + 
             AS_serve.returnpt_co:AS_return.servept_co + AS_rank_points:AS_return.servept + 
             AS_matchw:AS_matchw_os + 
             AS_return.servept_os:AS_return.serveg_os + 
             AS_return.servept_co:AS_matchw_co + 
             AS_serve.returng:AS_return.serveg + 
             AS_return.servept_os:AS_serve.returnpt_co_os + AS_return.servept:AS_serve.returng_co_os + 
             AS_serve.returnpt_co_os:AS_matchw_co_os +  
             AS_return.servept_co:AS_matchw_os + AS_left_hand_advan:AS_age + 
             AS_fatigue:AS_return.serveg_co_os + 
             AS_age:AS_serve.returng_os +
             AS_age:AS_serve.returnpt_co + AS_serve.returnpt_co_os:AS_serve.returng_co_os + 
             AS_return.serveg_os:AS_serve.returng_co_os + 
             AS_rank_points:surface.LR. + AS_serve.returnpt:AS_serve.returng + 
             AS_serve.returng_os:AS_return.serveg_os + AS_serve.returnpt:AS_return.serveg + 
             AS_return.serveg:AS_serve.returng_co + AS_matchw:surface.LR. + 
             AS_serve.returng_co_os:surface.LR. +  
             AS_serve.returnpt_co_os:AS_return.servept_co_os + AS_return.servept_co_os:AS_matchw + 
             AS_serve.returng_os:AS_return.serveg_co_os + AS_return.serveg_co_os:AS_matchw + 
             AS_fatigue:AS_return.serveg_os + AS_return.servept_os:AS_matchw_os + 
             AS_serve.returng_os:AS_matchw_co_os + AS_rank_points:AS_matchw_co + 
             AS_return.servept:AS_return.serveg_co_os + 
             AS_home_advan:AS_return.serveg_co + 
             AS_serve.returng_os:AS_serve.returng_hth + AS_serve.returng_os:AS_return.serveg_hth_os + 
             AS_serve.returnpt_hth:AS_serve.returnpt_co + 
             AS_home_advan:AS_return.servept + 
             AS_serve.returng:AS_serve.returnpt_os + AS_serve.returnpt:AS_serve.returng_co + 
             AS_hth_matchw:AS_hth_matchw_os + AS_serve.returnpt_hth:AS_hth_matchw + 
             AS_serve.returnpt_hth:AS_return.servept_hth + AS_return.serveg_hth:AS_serve.returnpt_hth_os + 
             AS_matchw_os:AS_rank + 
             AS_return.servept_co:AS_tbw + 
             AS_left_hand_advan:AS_return.serveg_hth_os + AS_left_hand_advan:AS_hth_matchw_os + 
             AS_return.serveg:AS_serve.returnpt_os + 
             AS_fatigue:AS_serve.returng_os + 
             AS_fatigue:AS_return.servept_co_os + 
             AS_left_hand_advan:AS_serve.returnpt_hth + AS_left_hand_advan:AS_return.servept_hth + 
             AS_home_advan:AS_return.serveg_co_os + AS_left_hand_advan:AS_serve.returng_hth_os, 
           family = binomial(link = logit), data = minnotrain)

summary(glm6)

qchisq(.95,879)


## to calulcate accuracy and betting returns
 
## first self validating against training set

pre = predict(glm6, minnotrain, type = "response")


## normal betting if prob >= 50% and taking accouht of poss few zero's for betting odds

acc = 0 
win = 0
bet = 0

for (i in 1:length(pre)){
  if((minnotrain[i, 185])==0){
    if((pre[i] < 0.50) & (minnotrain[i,141] > 0)){
      acc = acc + 1
      win = win + minnotrain[i,141] 
    }
    if (minnotrain[i,141] >0){
      bet = bet + 1
    }
  } else{
    if((pre[i] >= 0.50) & (minnotrain[i,141] > 0)){
      acc = acc + 1
      win = win + minnotrain[i,141]
    }
    if (minnotrain[i,141] >0){
      bet = bet + 1
    }
  }
}


acc = acc/bet

acc

win = win - bet

ret = win/bet

ret


## refraining betting unless high probs

bet = 0
acc = 0 
win = 0

for (i in 1:length(pre)){
  if((minnotrain[i,185])==0){
    if((pre[i] <= 0.20) & (minnotrain[i,141] > 0)){
      acc = acc + 1
      bet = bet + 1
      win = win + minnotrain[i,141] 
    }
    if ((pre[i] >0.80) & (minnotrain[i,141] > 0)){
      bet = bet + 1
    }
  } else{
    if((pre[i] >= 0.80) & (minnotrain[i,141] > 0)){
      acc = acc + 1
      win = win + minnotrain[i,141]
      bet = bet + 1
    } 
    if((pre[i] < 0.20) & (minnotrain[i,141] > 0)){
      bet = bet + 1
    }
  }
}



acc = acc/bet

acc

win = win - bet

ret = win/bet

ret


## to do grid search of above values to find any more optimal thresholds for use in test set run below

a = 0.50
b = 0.50

for (w in 1:45){
  
  
  bet = 0
  acc = 0 
  win = 0
  
  for (i in 1:length(pre)){
    if((minnotrain[i,185])==0){
      if((pre[i] <= a) & (minnotrain[i,141] > 0)){
        acc = acc + 1
        bet = bet + 1
        win = win + minnotrain[i,141] 
      }
      if ((pre[i] >b) & (minnotrain[i,141] > 0)){
        bet = bet + 1
      }
    } else{
      if((pre[i] >= b) & (minnotrain[i,141] > 0)){
        acc = acc + 1
        win = win + minnotrain[i,141]
        bet = bet + 1
      } 
      if((pre[i] < a) & (minnotrain[i,141] > 0)){
        bet = bet + 1
      }
    }
  }
  
  acc = acc/bet
  
  print(acc)
  
  win = win - bet
  
  print(win/bet)
  
  a = a - .01
  b = b + .01
  
}



## now test against test set 


pre = predict(glm6, minnotest, type = "response")

## normal betting if prob >= 50% and taking accouht of poss few zero's for betting odds

acc = 0 
win = 0
bet = 0


for (i in 1:length(pre)){
  if((minnotest[i, 185])==0){
    if((pre[i] < 0.50) & (minnotest[i,141] > 0)){
      acc = acc + 1
      win = win + minnotest[i,141] 
    }
    if (minnotest[i,141] >0){
      bet = bet + 1
    }
  } else{
    if((pre[i] >= 0.50) & (minnotest[i,141] > 0)){
      acc = acc + 1
      win = win + minnotest[i,141]
    }
    if (minnotest[i,141] >0){
      bet = bet + 1
    }
  }
}


acc = acc/bet

acc

win = win - bet

ret = win/bet

ret


## refraining betting unless high probs

bet = 0
acc = 0 
win = 0

for (i in 1:length(pre)){
  if((minnotest[i,185])==0){
    if((pre[i] <= 0.16) & (minnotest[i,141] > 0)){
      acc = acc + 1
      bet = bet + 1
      win = win + minnotest[i,141] 
    }
    if ((pre[i] >0.84) & (minnotest[i,141] > 0)){
      bet = bet + 1
    }
  } else{
    if((pre[i] >= 0.84) & (minnotest[i,141] > 0)){
      acc = acc + 1
      win = win + minnotest[i,141]
      bet = bet + 1
    } 
    if((pre[i] < 0.16) & (minnotest[i,141] > 0)){
      bet = bet + 1
    }
  }
}


acc = acc/bet

acc

win = win - bet

ret = win/bet

ret


## to calculate bookies returns using normal betting strategy above to compare with above or later testing

win =0
bet = 0

for (i in 1:2282){
  if (tenniste[i,141] < tenniste[i,142]){
    win = win + tenniste[i,141]
  }
  if (tenniste[i,141] > 0){
    bet = bet + 1
  }
}

bet

ret = (win - bet)/bet

ret



win = 0 
bet = 0

for (i in 1:438){
  if (minnotest[i,141] < minnotest[i,142]){
    win = win + minnotest[i,141]
  }
  if (minnotest[i,141] > 0){
    bet = bet + 1
  }
}

ret = (win - bet)/bet

ret


## to calculate bookies returns using high probs betting strategy above (80% thresholds) to compare with later  


win = 0 
bet = 0

for (i in 1:438){
  if ((minnotest[i,141] < 1.25) & (minnotest[i,141]>0)){
    win = win + minnotest[i,141]
    bet = bet + 1
  }
  if ((minnotest[i,142] < 1.25) & (minnotest[i,141]>0)){
    bet = bet + 1
  }
}

bet

ret = (win - bet)/bet

ret


win = 0 
bet = 0

for (i in 1:2282){
  if ((tenniste[i,141] < 1.25) & (tenniste[i,141] > 0)){
    win = win + tenniste[i,141]
    bet = bet + 1
  }
  if ((tenniste[i,142] < 1.25) & (tenniste[i,141] >0)){
    bet = bet + 1
  }
}

bet

ret = (win - bet)/bet

ret


## to calculate bookies returns using high probs betting strategy above (70% threshold) to compare with later  

win = 0 
bet = 0

for (i in 1:438){
  if ((minnotest[i,141] < 1.4286) & (minnotest[i,141]>0)){
    win = win + minnotest[i,141]
    bet = bet + 1
  }
  if ((minnotest[i,142] < 1.4286) & (minnotest[i,141]>0)){
    bet = bet + 1
  }
}

bet

ret = (win - bet)/bet

ret


win = 0 
bet = 0

for (i in 1:2282){
  if ((tenniste[i,141] < 1.4286) & (tenniste[i,141] > 0)){
    win = win + tenniste[i,141]
    bet = bet + 1
  }
  if ((tenniste[i,142] < 1.4286) & (tenniste[i,141] >0)){
    bet = bet + 1
  }
}

bet

ret = (win - bet)/bet

ret


## to calculate bookies returns using high probs betting strategy above (84%/89% thresholds) to compare with above/later  

win = 0 
bet = 0

for (i in 1:438){
  if ((minnotest[i,141] < 1.1905) & (minnotest[i,141]>0)){
    win = win + minnotest[i,141]
    bet = bet + 1
  }
  if ((minnotest[i,142] < 1.1905) & (minnotest[i,141]>0)){
    bet = bet + 1
  }
}


ret = (win - bet)/bet

ret


win = 0 
bet = 0

for (i in 1:2282){
  if ((tenniste[i,141] < 1.1236) & (tenniste[i,141] > 0)){
    win = win + tenniste[i,141]
    bet = bet + 1
  }
  if ((tenniste[i,142] < 1.1236) & (tenniste[i,141] > 0)){
    bet = bet + 1
  }
}



ret = (win - bet)/bet

ret


### to modify above train and test sets for use with other machine learning algorithms and to create standardised versions where required 


SLRtrain = LRtrain
SLRtest = LRtest

SLRtrain1 = SLRtrain[,-c(1:3, 38,39,41)]
SLRtest1 = SLRtest[,-c(1:3, 38,39,41)]

SLRtrain1 = scale(SLRtrain1)
SLRtest1 = scale(SLRtest1)


for(i in 4:37){
  SLRtrain[,i] = SLRtrain1[,(i-3)]
}

for(i in 4:37){
  SLRtest[,i] = SLRtest1[,(i-3)]
}

SLRtrain[,40] = SLRtrain1[,35]
SLRtest[,40] = SLRtest1[,35]

SLRtrain[,41] = LRtrain[,41]
SLRtest[,41] = LRtest[,41]


MLtrain = LRtrain
MLtest = LRtest
SMLtrain = SLRtrain
SMLtest = SLRtest

MLtrain = MLtrain[,-39]
MLtest = MLtest[,-39]
SMLtrain = SMLtrain[,-39]
SMLtest = SMLtest[,-39]

MLtrain[,41] = tennistr[,144]
MLtest[,41] = tenniste[,144]
SMLtrain[,41] = tennistr[,144]
SMLtest[,41] = tenniste[,144]

MLtrain[,42] = tennistr[,145]
MLtest[,42] = tenniste[,145]
SMLtrain[,42] = tennistr[,145]
SMLtest[,42] = tenniste[,145]

MLtrain[,43] = tennistr[,146]
MLtest[,43] = tenniste[,146]
SMLtrain[,43] = tennistr[,146]
SMLtest[,43] = tenniste[,146]


MLmtr = minnotrain
MLmte = minnotest

MLmtr = MLmtr[,-c(1:142)]
MLmte = MLmte[,-c(1:142)]


SMLmtr = MLmtr
SMLmte = MLmte

SMLmtr1 = SMLmtr[,-c(1:6,41,42,43,44)]
SMLmte1 = SMLmte[,-c(1:6,41,42,43,44)]

SMLmtr1 = scale(SMLmtr1)
SMLmte1 = scale(SMLmte1)



for(i in 7:40){
  SMLmtr[,i] = SMLmtr1[,(i-6)]
}

for(i in 7:40){
  SMLmte[,i] = SMLmte1[,(i-6)]
}



SMLmtr[,45] = SMLmtr1[,35]
SMLmte[,45] = SMLmte1[,35]


SMLmtr = SMLmtr[,-c(41,42,44)]
SMLmte = SMLmte[,-c(41,42,44)]

MLmtr = MLmtr[,-c(41,42,44)]
MLmte = MLmte[,-c(41,42,44)]

MLtrain = MLtrain[,-40]
SMLtrain = SMLtrain[,-40]
SMLtest = SMLtest[,-40]
MLtest = MLtest[,-40]

path = "D:\\MLtrain.csv"
write.csv(MLtrain, path)

path = "D:\\SMLtrain.csv"
write.csv(SMLtrain, path)

path = "D:\\MLtest.csv"
write.csv(MLtest, path)

path = "D:\\SMLtest.csv"
write.csv(SMLtest, path)

path = "D:\\MLmtr.csv"
write.csv(MLmtr, path)

path = "D:\\MLmte.csv"
write.csv(MLmte, path)

path = "D:\\SMLmtr.csv"
write.csv(SMLmtr, path)

path = "D:\\SMLmte.csv"
write.csv(SMLmte, path)



## Machine Learning Algorithm 1 :  Random Forests 

library(mlr)


### feature selection

library(randomForest)

fit = randomForest(AS_win_lose_binary ~., data = MLtrain, importance = TRUE)

importance(fit)


MLtrain1 = MLtrain

MLtrain1 = MLtrain1[, -c(1,2,3,19,20,21,22,33,34,40,41,42)]

MLtest1 = MLtest

MLtest1 = MLtest1[, -c(1,2,3,19,20,21,22,33,34,40,41,42)]


#### grid search for best ntree and mtry parameters 

set.seed(25)
learner = makeLearner("classif.randomForest")

ps = makeParamSet(makeIntegerParam("ntree", lower = 10, upper = 200), makeIntegerParam("mtry", lower = 4, upper = 25))

ctrl = makeTuneControlGrid()

task = makeClassifTask(data = MLtrain1, target ="AS_win_lose_binary")
rdesc= makeResampleDesc("CV", iters = 3)

res = tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = mmce)



### do revised model on test set with optimal parameters from above search 

set.seed(26)
learner = makeLearner("classif.randomForest", ntree = 200, mtry = 4, predict.type = "prob")

task = makeClassifTask(data = MLtrain1, target ="AS_win_lose_binary")

train.set = c(1:nrow(MLtrain1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = MLtest1)

accuracy = performance(prediction, measures = mmce)
print(accuracy)

print(calculateConfusionMatrix(prediction))

rdesc= makeResampleDesc("CV", iters = 10)
result= resample("classif.randomForest", ntree = 200, mtry = 4, task, rdesc, measures = mmce)
print(result)


### generate roc curve

df = generateThreshVsPerfData(prediction, measures = list(fpr, tpr))
plotROCCurves(df)
print(df)

plotThreshVsPerf(df)


## returns using bet on favourite Betting strategy 

favbsf = function(){

  bet = 0
  win = 0
  
  for (i in 1:2282){
    if((MLtest1[i, 29])==0){
      if((prediction$data[i,3] < 0.50) & (tenniste[i,141] > 0)){
        win = win + tenniste[i,141] 
      }
      if (tenniste[i,141] >0){
        bet = bet + 1
      }
    } else{
      if((prediction$data[i,3] >= 0.50) & (tenniste[i,141] > 0)){
        win = win + tenniste[i,141]
      }
      if (tenniste[i,141] >0){
        bet = bet + 1
      }
    }
  }
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

favbsf()


## refraining betting unless high probs

hpbsf = function(){

  bet = 0
  acc = 0 
  win = 0
  
  for (i in 1:2282){
    if((MLtest1[i,29])==0){
      if((prediction$data[i,3] <= 0.20) & (tenniste[i,141] > 0)){
        acc = acc + 1
        bet = bet + 1
        win = win + tenniste[i,141] 
      }
      if ((prediction$data[i,3] >0.80) & (tenniste[i,141] > 0)){
        bet = bet + 1
      }
    } else{
      if((prediction$data[i,3] >= 0.80) & (tenniste[i,141] > 0)){
        acc = acc + 1
        win = win + tenniste[i,141]
        bet = bet + 1
      } 
      if((prediction$data[i,3] < 0.20) & (tenniste[i,141] > 0)){
        bet = bet + 1
      }
    }
  }
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

hpbsf()


### Do all of the above using reduced minimum numbers filtered model

### feature selection


fit = randomForest(AS_win_lose_binary ~., data = MLmtr, importance = TRUE)

importance(fit)

View(colnames(MLmtr1))

MLmtr1 = MLmtr

MLmtr1 = MLmtr1[, -c(1,2,3,4,5,6,22,23,24,25,36,37)]

MLmte1 = MLmte

MLmte1 = MLmte1[, -c(1,2,3,4,5,6,22,23,24,25,36,37)]


#### grid search for best ntree and mtry parameters 

set.seed(31)
learner = makeLearner("classif.randomForest")

ps = makeParamSet(makeIntegerParam("ntree", lower = 10, upper = 200), makeIntegerParam("mtry", lower = 4, upper = 25))

ctrl = makeTuneControlGrid()

task = makeClassifTask(data = MLmtr1, target ="AS_win_lose_binary")
rdesc= makeResampleDesc("CV", iters = 3)

res = tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = mmce)


### do revised model on test set with optimal parameters from above search 

set.seed(32)
learner = makeLearner("classif.randomForest", ntree = 179, mtry = 6, predict.type = "prob")

task = makeClassifTask(data = MLmtr1, target ="AS_win_lose_binary")

train.set = c(1:nrow(MLmtr1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = MLmte1)

accuracy = performance(prediction, measures = mmce)
print(accuracy)

print(calculateConfusionMatrix(prediction))

rdesc= makeResampleDesc("CV", iters = 10)
result= resample("classif.randomForest", ntree = 179, mtry = 6, task, rdesc, measures = mmce)
print(result)


### generate roc curve

df = generateThreshVsPerfData(prediction, measures = list(fpr, tpr, auc))
plotROCCurves(df)
print(df)

plotThreshVsPerf(df)


## returns using bet on favourite Betting strategy 

favbsfil = function(){

  bet = 0
  win = 0
  
  for (i in 1:438){
    if((MLmte1[i, 29])==0){
      if((prediction$data[i,3] < 0.50) & (minnotest[i,141] > 0)){
        win = win + minnotest[i,141] 
      }
      if (minnotest[i,141] >0){
        bet = bet + 1
      }
    } else{
      if((prediction$data[i,3] >= 0.50) & (minnotest[i,141] > 0)){
        win = win + minnotest[i,141]
      }
      if (minnotest[i,141] >0){
        bet = bet + 1
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

favbsfil()


## refraining betting unless high probs

hpbsfil = function(){

  bet = 0
  acc = 0 
  win = 0
  
  for (i in 1:438){
    if((MLmte1[i,29])==0){
      if((prediction$data[i,3] <= 0.2) & (minnotest[i,141] > 0)){
        acc = acc + 1
        bet = bet + 1
        win = win + minnotest[i,141] 
      }
      if ((prediction$data[i,3] >0.8) & (minnotest[i,141] > 0)){
        bet = bet + 1
      }
    } else{
      if((prediction$data[i,3] >= 0.8) & (minnotest[i,141] > 0)){
        acc = acc + 1
        win = win + minnotest[i,141]
        bet = bet + 1
      } 
      if((prediction$data[i,3] < 0.2) & (minnotest[i,141] > 0)){
        bet = bet + 1
      }
    }
  }
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

hpbsfil()



## Machine Learning Algorithm 2 :  Support Vector Machines 

# feature selection

learner = makeLearner("classif.svm")
task = makeClassifTask(data = SMLtrain, target ="AS_win_lose_binary")


ctrl = makeFeatSelControlSequential(method = "sbs", beta = -1, max.features = 20)

rdesc = makeResampleDesc("Holdout", split = 0.3)

res = selectFeatures(learner, task, rdesc, control = ctrl)
print(analyzeFeatSelResult(res))

View(colnames(SMLtest1))

SMLtrain1 = SMLtrain

SMLtrain1 = SMLtrain1[, -c(1,8,9,17,19,20,22,25,37,40,41,42)]

SMLtest1 = SMLtest

SMLtest1 = SMLtest1[, -c(1,8,9,17,19,20,22,25,37,40,41,42)]


### select optimal kernel type

svm_retest = function(kern){
  
  learner = makeLearner("classif.svm", kernel = kern)
  task = makeClassifTask(data = SMLtrain1, target ="AS_win_lose_binary")
  
  rdesc= makeResampleDesc("CV", iters = 10)
  
  result= resample("classif.svm", task, rdesc, measures = mmce)
  print(result)
  
  
}

svm_retest("polynomial")

svm_retest("sigmoid")

svm_retest("linear")

svm_retest("radial")


#### grid search for best cost and nu parameters and with kernel from above  

set.seed(40)
learner = makeLearner("classif.svm", kernel = "polynomial")

ps = makeParamSet(makeIntegerParam("cost", lower = 0.6, upper = 10), makeIntegerParam("nu", lower = -5, upper = 10))

ctrl = makeTuneControlGrid()

task = makeClassifTask(data = SMLtrain1, target ="AS_win_lose_binary")
rdesc= makeResampleDesc("CV", iters = 3)

res = tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = mmce)


### test model based on results of above 

set.seed(41)
learner = makeLearner("classif.svm", predict.type = "prob", cost = 4, nu = 8, kernel = "polynomial")

task = makeClassifTask(data = SMLtrain1, target ="AS_win_lose_binary")

train.set = c(1:nrow(SMLtrain1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = SMLtest1)

accuracy = performance(prediction, measures = mmce)
print(accuracy)

print(calculateConfusionMatrix(prediction))

rdesc= makeResampleDesc("CV", iters = 10)
result= resample("classif.svm", cost = 4, nu = 8, kernel = "polynomial", task, rdesc, measures = mmce)
print(result)


### generate roc curve

df = generateThreshVsPerfData(prediction, measures = list(fpr, tpr))
plotROCCurves(df)
print(df)

plotThreshVsPerf(df)


## returns using bet on favourite Betting strategy 

favbsf()


## refraining betting unless high probs

hpbsf()


### Do all of the above using reduced minimum numbers filtered model

### feature selection

learner = makeLearner("classif.svm")
task = makeClassifTask(data = SMLmtr, target ="AS_win_lose_binary")


ctrl = makeFeatSelControlSequential(method = "sbs", beta = -1, max.features = 20)

rdesc = makeResampleDesc("Holdout", split = 0.3)

res = selectFeatures(learner, task, rdesc, control = ctrl)
print(analyzeFeatSelResult(res))

View(colnames(SMLmtr))

SMLmtr1 = SMLmtr

SMLmtr1 = SMLmtr1[, -c(2,3,4,5,10,13,21,22,23,25,40,42)]

SMLmte1 = SMLmte

SMLmte1 = SMLmte1[, -c(2,3,4,5,10,13,21,22,23,25,40,42)]


### select optimal kernel type

svm_retest1 = function(kern){
  
  learner = makeLearner("classif.svm", kernel = kern)
  task = makeClassifTask(data = SMLmtr1, target ="AS_win_lose_binary")
  
  rdesc= makeResampleDesc("CV", iters = 10)
  
  result= resample("classif.svm", task, rdesc, measures = mmce)
  print(result)
  
  
}

svm_retest1("polynomial")

svm_retest1("sigmoid")

svm_retest1("linear")

svm_retest1("radial")


#### grid search for best cost and nu parameters and with kernel from above  

set.seed(42)
learner = makeLearner("classif.svm", kernel = "linear")

ps = makeParamSet(makeIntegerParam("cost", lower = 0.6, upper = 10), makeIntegerParam("nu", lower = -5, upper = 10))

ctrl = makeTuneControlGrid()

task = makeClassifTask(data = SMLmtr1, target ="AS_win_lose_binary")
rdesc= makeResampleDesc("CV", iters = 3)

res = tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = mmce)


### test model based on results of above 

set.seed(43)
learner = makeLearner("classif.svm", predict.type = "prob", cost = 1, nu = 0, kernel = "linear")

task = makeClassifTask(data = SMLmtr1, target ="AS_win_lose_binary")

train.set = c(1:nrow(SMLmtr1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = SMLmte1)

accuracy = performance(prediction, measures = mmce)
print(accuracy)

print(calculateConfusionMatrix(prediction))

rdesc= makeResampleDesc("CV", iters = 10)
result= resample("classif.svm", cost = 1, nu = 0 , kernel = "linear", task, rdesc, measures = mmce)
print(result)


### generate roc curve  


df = generateThreshVsPerfData(prediction, measures = list(fpr, tpr))
plotROCCurves(df)
print(df)

plotThreshVsPerf(df)


## returns using bet on favourite Betting strategy 

favbsfil()

## refraining betting unless high probs

hpbsfil()



## New Betting and Money Management strategies for Random Forest Full Data Model 

# Recreate predcit values for Random Forest Full Data Model 

set.seed(26)
learner = makeLearner("classif.randomForest", ntree = 200, mtry = 4, predict.type = "prob")

task = makeClassifTask(data = MLtrain1, target ="AS_win_lose_binary")

train.set = c(1:nrow(MLtrain1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = MLtest1)


#BS1  bet if expected postive return

eprbsf = function(){

  bet = 0
  win = 0
  
  for (i in 1:2282){
    if((MLtest1[i,29])==0){
      if((prediction$data[i,3]*tenniste[i,142]) > 1){
        bet = bet + 1
      } 
      if(((1- prediction$data[i,3])*tenniste[i,141]) >1){
        bet = bet + 1
        win = win + tenniste[i,141]
      }
    } else{
      if((prediction$data[i,3]*tenniste[i,141]) > 1){
        win = win + tenniste[i,141]
        bet = bet + 1
      } 
      if(((1- prediction$data[i,3])*tenniste[i,142]) >1){
        bet = bet + 1
      }
    }
  }
  
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

eprbsf()


#BS2 bet model prob - bookmaker prob >= 0.05

mprbsf = function(){

  bet = 0
  win = 0
  
  for (i in 1:2282){
    if((MLtest1[i,29])==0){
      if(((prediction$data[i,3] - (1/tenniste[i,142])) >=  0.05) & (tenniste[i,142] > 0)){
        bet = bet + 1
      }
      if((((1-prediction$data[i,3]) - (1/tenniste[i,141])) >= 0.05) & (tenniste[i,141] > 0)){
        bet = bet + 1
        win = win + tenniste[i,141]
      }
    } else{
      if(((prediction$data[i,3] - (1/tenniste[i,141])) >=  0.05) & (tenniste[i,141] > 0)){
        win = win + tenniste[i,141]
        bet = bet + 1
      }
      if((((1-prediction$data[i,3]) - (1/tenniste[i,142])) >= 0.05) & (tenniste[i,142] > 0)){
        bet = bet + 1
      } 
    }
  }
  
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

mprbsf()


#MM Strategy #1 - BS#1 with Kelly Criterion

eprkcf = function(){

  bet = 0
  win = 0
  
  for (i in 1:2282){
    stake1 = ((prediction$data[i,3]*tenniste[i,142])-1)/(tenniste[i,142]-1)
    stake3 = ((prediction$data[i,3]*tenniste[i,141])-1)/(tenniste[i,141]-1)
    stake2 = (((1-prediction$data[i,3])*tenniste[i,141])-1)/(tenniste[i,141]-1)
    stake4 = (((1-prediction$data[i,3])*tenniste[i,142])-1)/(tenniste[i,142]-1)
    if((MLtest1[i,29])==0){
      if((prediction$data[i,3]*tenniste[i,142]) > 1){
        bet = bet + stake1
      } 
      if(((1- prediction$data[i,3])*tenniste[i,141]) >1){
        bet = bet + stake2
        win = win + (stake2*tenniste[i,141]) 
      }
    } else{
      if((prediction$data[i,3]*tenniste[i,141]) > 1){
        win = win + (stake3*tenniste[i,141])
        bet = bet + stake3
      } 
      if(((1- prediction$data[i,3])*tenniste[i,142]) >1){
        bet = bet + stake4
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

eprkcf()


#MM Strategy #1 - BS#2 with Kelly Criterion

mprkcf = function(){
  
  bet = 0
  win = 0
  
  for (i in 1:2282){
    stake1 = ((prediction$data[i,3]*tenniste[i,142])-1)/(tenniste[i,142]-1)
    stake3 = ((prediction$data[i,3]*tenniste[i,141])-1)/(tenniste[i,141]-1)
    stake2 = (((1-prediction$data[i,3])*tenniste[i,141])-1)/(tenniste[i,141]-1)
    stake4 = (((1-prediction$data[i,3])*tenniste[i,142])-1)/(tenniste[i,142]-1)
    if((MLtest1[i,29])==0){
      if(((prediction$data[i,3] - (1/tenniste[i,142])) >=  0.05) & (tenniste[i,142] > 0)){
        bet = bet + stake1
      } 
      if((((1-prediction$data[i,3]) - (1/tenniste[i,141])) >= 0.05) & (tenniste[i,141] > 0)){
        bet = bet + stake2
        win = win + (stake2*tenniste[i,141]) 
      }
    } else{
      if(((prediction$data[i,3] - (1/tenniste[i,141])) >=  0.05) & (tenniste[i,141] > 0)){
        win = win + (stake3*tenniste[i,141])
        bet = bet + stake3
      } 
      if((((1-prediction$data[i,3]) - (1/tenniste[i,142])) >= 0.05) & (tenniste[i,142] > 0)){
        bet = bet + stake4
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

mprkcf()


#MM Strategy #2 - BS#1 with Kelly Criterion reversed

eprrkcf = function(){

  bet = 0
  win = 0
  
  for (i in 1:2282){
    stake1 = 1 - ((prediction$data[i,3]*tenniste[i,142])-1)/(tenniste[i,142]-1)
    stake3 = 1 - ((prediction$data[i,3]*tenniste[i,141])-1)/(tenniste[i,141]-1)
    stake2 = 1 - (((1-prediction$data[i,3])*tenniste[i,141])-1)/(tenniste[i,141]-1)
    stake4 = 1 - (((1-prediction$data[i,3])*tenniste[i,142])-1)/(tenniste[i,142]-1)
    if((MLtest1[i,29])==0){
      if((prediction$data[i,3]*tenniste[i,142]) > 1){
        bet = bet + stake1
      } 
      if(((1- prediction$data[i,3])*tenniste[i,141]) >1){
        bet = bet + stake2
        win = win + (stake2*tenniste[i,141]) 
      }
    } else{
      if((prediction$data[i,3]*tenniste[i,141]) > 1){
        win = win + (stake3*tenniste[i,141])
        bet = bet + stake3
      } 
      if(((1- prediction$data[i,3])*tenniste[i,142]) >1){
        bet = bet + stake4
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

eprrkcf()


#MM Strategy #2 - BS#2 with Kelly Criterion reversed

mprrkcf = function(){

  bet = 0
  win = 0
  
  for (i in 1:2282){
    stake1 = 1 - ((prediction$data[i,3]*tenniste[i,142])-1)/(tenniste[i,142]-1)
    stake3 = 1 - ((prediction$data[i,3]*tenniste[i,141])-1)/(tenniste[i,141]-1)
    stake2 = 1 - (((1-prediction$data[i,3])*tenniste[i,141])-1)/(tenniste[i,141]-1)
    stake4 = 1 - (((1-prediction$data[i,3])*tenniste[i,142])-1)/(tenniste[i,142]-1)
    if((MLtest1[i,29])==0){
      if(((prediction$data[i,3] - (1/tenniste[i,142])) >=  0.05) & (tenniste[i,142] > 0)){
        bet = bet + stake1
      } 
      if((((1-prediction$data[i,3]) - (1/tenniste[i,141])) >= 0.05) & (tenniste[i,141] > 0)){
        bet = bet + stake2
        win = win + (stake2*tenniste[i,141]) 
      }
    } else{
      if(((prediction$data[i,3] - (1/tenniste[i,141])) >=  0.05) & (tenniste[i,141] > 0)){
        win = win + (stake3*tenniste[i,141])
        bet = bet + stake3
      } 
      if((((1-prediction$data[i,3]) - (1/tenniste[i,142])) >= 0.05) & (tenniste[i,142] > 0)){
        bet = bet + stake4
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

mprrkcf()


# Betting strategies from first part of project excluded here as will not always work with Kelly Criterion

#MM Strategy #3 - BB#1 with % of Bankroll Criterion

eprbcf = function(){

  nb = 0
  bet = 0
  win = 0
  BR = 2282
  
  for (i in 1:2282){
    stake = 0.01*BR
    if((MLtest1[i,29])==0){
      if((prediction$data[i,3]*tenniste[i,142]) > 1){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      } 
      if(((1- prediction$data[i,3])*tenniste[i,141]) >1){
        bet = bet + stake
        win = win + (stake*tenniste[i,141])
        BR = BR + ((stake*tenniste[i,141]) - stake)
        nb = nb + 1
      }
    } else{
      if((prediction$data[i,3]*tenniste[i,141]) > 1){
        win = win + (stake*tenniste[i,141])
        bet = bet + stake
        BR = BR + ((stake*tenniste[i,141]) - stake)
        nb = nb + 1
      } 
      if(((1- prediction$data[i,3])*tenniste[i,142]) >1){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    }
    if (BR <=0){
      break
    }
  }
  
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

eprbcf()


#MM Strategy #3 - BS#2 with % of Bankroll Criterion

mprbcf = function(){

  nb = 0
  bet = 0
  win = 0
  BR = 2282
  
  for (i in 1:2282){
    stake = 0.01*BR
    if((MLtest1[i,29])==0){
      if(((prediction$data[i,3] - (1/tenniste[i,142])) >=  0.05) & (tenniste[i,142] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      } 
      if((((1-prediction$data[i,3]) - (1/tenniste[i,141])) >= 0.05) & (tenniste[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*tenniste[i,141])
        BR = BR + ((stake*tenniste[i,141]) - stake)
        nb = nb + 1
      }
    } else{
      if(((prediction$data[i,3] - (1/tenniste[i,141])) >=  0.05) & (tenniste[i,141] > 0)){
        win = win + (stake*tenniste[i,141])
        bet = bet + stake
        BR = BR + ((stake*tenniste[i,141]) - stake)
        nb = nb + 1
      } 
      if((((1-prediction$data[i,3]) - (1/tenniste[i,142])) >= 0.05) & (tenniste[i,142] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    }
    if (BR <=0){
      break
    }
  }
  
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

mprbcf()



#MM Strategy #3 - bet on favourite betting staregy in first part of project with % of Bankroll Criterion

favbcf = function(){

  nb = 0
  bet = 0
  win = 0
  BR = 2282
  
  
  for (i in 1:2282){
    stake = 0.01*BR
    if((MLtest1[i, 29])==0){
      if((prediction$data[i,3] <= 0.50) & (tenniste[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*tenniste[i,141])
        BR = BR + ((stake*tenniste[i,141]) - stake)
        nb = nb + 1
      }
      if((prediction$data[i,3] > 0.50) & (tenniste[i,141] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    } else{
      if((prediction$data[i,3] >= 0.50) & (tenniste[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*tenniste[i,141])
        BR = BR + ((stake*tenniste[i,141]) - stake)
        nb = nb + 1
      }
      if((prediction$data[i,3] < 0.50) & (tenniste[i,141] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    }
    if (BR <=0){
      break
    }
  }
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

favbcf()

  
#MM Strategy #3 - bet on high probabilities betting staregy in first part of project with % of Bankroll Criterion

hpbcf = function(){

  nb = 0
  bet = 0
  win = 0
  BR = 2282
  
  for (i in 1:2282){
    stake = 0.01*BR
    if((MLtest1[i, 29])==0){
      if((prediction$data[i,3] <= 0.20) & (tenniste[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*tenniste[i,141])
        BR = BR + ((stake*tenniste[i,141]) - stake)
        nb = nb + 1
      }
      if((prediction$data[i,3] > 0.80) & (tenniste[i,141] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    } else{
      if((prediction$data[i,3] >= 0.80) & (tenniste[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*tenniste[i,141])
        BR = BR + ((stake*tenniste[i,141]) - stake)
        nb = nb + 1
      }
      if((prediction$data[i,3] < 0.20) & (tenniste[i,141] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    }
    if (BR <=0){
      break
    }  
  }
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

hpbcf()


## New Betting and Money Management strategies for Random Forest Filtered Data Model 

# Recreate predict value for Random Forest Filtered Data Model

set.seed(32)
learner = makeLearner("classif.randomForest", ntree = 179, mtry = 6, predict.type = "prob")

task = makeClassifTask(data = MLmtr1, target ="AS_win_lose_binary")

train.set = c(1:nrow(MLmtr1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = MLmte1)



#BS1  bet if expected postive return

eprbsfil = function(){

  bet = 0
  win = 0
  
  for (i in 1:438){
    if((MLmte1[i,29])==0){
      if((prediction$data[i,3]*minnotest[i,142]) > 1){
        bet = bet + 1
      } 
      if(((1- prediction$data[i,3])*minnotest[i,141]) >1){
        bet = bet + 1
        win = win + minnotest[i,141]
      }
    } else{
      if((prediction$data[i,3]*minnotest[i,141]) > 1){
        win = win + minnotest[i,141]
        bet = bet + 1
      } 
      if(((1- prediction$data[i,3])*minnotest[i,142]) >1){
        bet = bet + 1
      }
    }
  }
  
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

eprbsfil()



#BS2 bet model prob - bookmaker prob >= 0.05

mprbsfil = function(){

  bet = 0
  win = 0
  
  for (i in 1:438){
    if((MLmte1[i,29])==0){
      if(((prediction$data[i,3] - (1/minnotest[i,142])) >=  0.05) & (minnotest[i,142] > 0)){
        bet = bet + 1
      }
      if((((1-prediction$data[i,3]) - (1/minnotest[i,141])) >= 0.05) & (minnotest[i,141] > 0)){
        bet = bet + 1
        win = win + minnotest[i,141]
      }
    } else{
      if(((prediction$data[i,3] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,141] > 0)){
        win = win + minnotest[i,141]
        bet = bet + 1
      }
      if((((1-prediction$data[i,3]) - (1/minnotest[i,142])) >= 0.05) & (minnotest[i,142] > 0)){
        bet = bet + 1
      } 
    }
  }
  
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

mprbsfil()


#MM Strategy #1 - BS#1 with Kelly Criterion

eprkcfil = function(){

  bet = 0
  win = 0
  
  for (i in 1:438){
    stake1 = ((prediction$data[i,3]*minnotest[i,142])-1)/(minnotest[i,142]-1)
    stake3 = ((prediction$data[i,3]*minnotest[i,141])-1)/(minnotest[i,141]-1)
    stake2 = (((1-prediction$data[i,3])*minnotest[i,141])-1)/(minnotest[i,141]-1)
    stake4 = (((1-prediction$data[i,3])*minnotest[i,142])-1)/(minnotest[i,142]-1)
    if((MLmte1[i,29])==0){
      if((prediction$data[i,3]*minnotest[i,142]) > 1){
        bet = bet + stake1
      } 
      if(((1- prediction$data[i,3])*minnotest[i,141]) >1){
        bet = bet + stake2
        win = win + (stake2*minnotest[i,141]) 
      }
    } else{
      if((prediction$data[i,3]*minnotest[i,141]) > 1){
        win = win + (stake3*minnotest[i,141])
        bet = bet + stake3
      } 
      if(((1- prediction$data[i,3])*minnotest[i,142]) >1){
        bet = bet + stake4
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

eprkcfil()


#MM Strategy #1 - BS#2 with Kelly Criterion

mprkcfil = function(){

  bet = 0
  win = 0
  
  for (i in 1:438){
    stake1 = ((prediction$data[i,3]*minnotest[i,142])-1)/(minnotest[i,142]-1)
    stake3 = ((prediction$data[i,3]*minnotest[i,141])-1)/(minnotest[i,141]-1)
    stake2 = (((1-prediction$data[i,3])*minnotest[i,141])-1)/(minnotest[i,141]-1)
    stake4 = (((1-prediction$data[i,3])*minnotest[i,142])-1)/(minnotest[i,142]-1)
    if((MLmte1[i,29])==0){
      if(((prediction$data[i,3] - (1/minnotest[i,142])) >=  0.05) & (minnotest[i,142] > 0)){
        bet = bet + stake1
      } 
      if((((1-prediction$data[i,3]) - (1/minnotest[i,141])) >= 0.05) & (minnotest[i,141] > 0)){
        bet = bet + stake2
        win = win + (stake2*minnotest[i,141]) 
      }
    } else{
      if(((prediction$data[i,3] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,141] > 0)){
        win = win + (stake3*minnotest[i,141])
        bet = bet + stake3
      } 
      if((((1-prediction$data[i,3]) - (1/minnotest[i,142])) >= 0.05) & (minnotest[i,142] > 0)){
        bet = bet + stake4
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

mprkcfil()


#MM Strategy #2 - BS#1 with Kelly Criterion reversed

eprrkcfil = function(){

  bet = 0
  win = 0
  
  for (i in 1:438){
    stake1 = 1 - ((prediction$data[i,3]*minnotest[i,142])-1)/(minnotest[i,142]-1)
    stake3 = 1 - ((prediction$data[i,3]*minnotest[i,141])-1)/(minnotest[i,141]-1)
    stake2 = 1 - (((1-prediction$data[i,3])*minnotest[i,141])-1)/(minnotest[i,141]-1)
    stake4 = 1 - (((1-prediction$data[i,3])*minnotest[i,142])-1)/(minnotest[i,142]-1)
    if((MLmte1[i,29])==0){
      if((prediction$data[i,3]*minnotest[i,142]) > 1){
        bet = bet + stake1
      } 
      if(((1- prediction$data[i,3])*minnotest[i,141]) >1){
        bet = bet + stake2
        win = win + (stake2*minnotest[i,141]) 
      }
    } else{
      if((prediction$data[i,3]*minnotest[i,141]) > 1){
        win = win + (stake3*minnotest[i,141])
        bet = bet + stake3
      } 
      if(((1- prediction$data[i,3])*minnotest[i,142]) >1){
        bet = bet + stake4
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

eprrkcfil()



#MM Strategy #2 - BS#2 with Kelly Criterion reversed

mprrkcfil = function(){

  bet = 0
  win = 0
  
  for (i in 1:438){
    stake1 = 1 - ((prediction$data[i,3]*minnotest[i,142])-1)/(minnotest[i,142]-1)
    stake3 = 1 - ((prediction$data[i,3]*minnotest[i,141])-1)/(minnotest[i,141]-1)
    stake2 = 1 - (((1-prediction$data[i,3])*minnotest[i,141])-1)/(minnotest[i,141]-1)
    stake4 = 1 - (((1-prediction$data[i,3])*minnotest[i,142])-1)/(minnotest[i,142]-1)
    if((MLmte1[i,29])==0){
      if(((prediction$data[i,3] - (1/minnotest[i,142])) >=  0.05) & (minnotest[i,142] > 0)){
        bet = bet + stake1
      } 
      if((((1-prediction$data[i,3]) - (1/minnotest[i,141])) >= 0.05) & (minnotest[i,141] > 0)){
        bet = bet + stake2
        win = win + (stake2*minnotest[i,141]) 
      }
    } else{
      if(((prediction$data[i,3] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,141] > 0)){
        win = win + (stake3*minnotest[i,141])
        bet = bet + stake3
      } 
      if((((1-prediction$data[i,3]) - (1/minnotest[i,142])) >= 0.05) & (minnotest[i,142] > 0)){
        bet = bet + stake4
      }
    }
  }
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

mprrkcfil()



# Betting strategies from first part of project excluded here as will not always work with Kelly Criterion


#MM Strategy #3 - BS#1 with % of Bankroll Criterion

eprbcfil = function(){

  nb = 0
  bet = 0
  win = 0
  BR = 438
  
  for (i in 1:438){
    stake = 0.01*BR
    if((MLmte1[i,29])==0){
      if((prediction$data[i,3]*minnotest[i,142]) > 1){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      } 
      if(((1- prediction$data[i,3])*minnotest[i,141]) >1){
        bet = bet + stake
        win = win + (stake*minnotest[i,141])
        BR = BR + ((stake*minnotest[i,141]) - stake)
        nb = nb + 1
      }
    } else{
      if((prediction$data[i,3]*minnotest[i,141]) > 1){
        win = win + (stake*minnotest[i,141])
        bet = bet + stake
        BR = BR + ((stake*minnotest[i,141]) - stake)
        nb = nb + 1
      } 
      if(((1- prediction$data[i,3])*minnotest[i,142]) >1){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    }
    if (BR <=0){
      break
    }
  }
  
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

eprbcfil()


#MM Strategy #3 - BS#2 with % of Bankroll Criterion

mprbcfil = function(){

  nb = 0
  bet = 0
  win = 0
  BR = 438
  
  for (i in 1:438){
    stake = 0.01*BR
    if((MLmte1[i,29])==0){
      if(((prediction$data[i,3] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,142] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      } 
      if((((1-prediction$data[i,3]) - (1/minnotest[i,141])) >= 0.05) & (minnotest[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*minnotest[i,141])
        BR = BR + ((stake*minnotest[i,141]) - stake)
        nb = nb + 1
      }
    } else{
      if(((prediction$data[i,3] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,141] > 0)){
        win = win + (stake*minnotest[i,141])
        bet = bet + stake
        BR = BR + ((stake*minnotest[i,141]) - stake)
        nb = nb + 1
      } 
      if((((1-prediction$data[i,3]) - (1/minnotest[i,142])) >= 0.05) & (minnotest[i,142] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    }
    if (BR <=0){
      break
    }
  }
  
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

mprbcfil()


#MM Strategy #3 - bet on favourite betting staregy in first part of project with % of Bankroll Criterion


favbcfil = function(){

  nb = 0
  bet = 0
  win = 0
  BR = 438
  
  
  for (i in 1:438){
    stake = 0.01*BR
    if((MLmte1[i, 29])==0){
      if((prediction$data[i,3] <= 0.50) & (minnotest[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*minnotest[i,141])
        BR = BR + ((stake*minnotest[i,141]) - stake)
        nb = nb + 1
      }
      if((prediction$data[i,3] > 0.50) & (minnotest[i,141] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    } else{
      if((prediction$data[i,3] >= 0.50) & (minnotest[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*minnotest[i,141])
        BR = BR + ((stake*minnotest[i,141]) - stake)
        nb = nb + 1
      }
      if((prediction$data[i,3] < 0.50) & (minnotest[i,141] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    }
    if (BR <=0){
      break
    }
  }
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

favbcfil()


#MM Strategy #3 - bet on high probabilities betting staregy in first part of project with % of Bankroll Criterion

hpbcfil = function(){

  nb = 0
  bet = 0
  win = 0
  BR = 438
  
  for (i in 1:438){
    stake = 0.01*BR
    if((MLmte1[i, 29])==0){
      if((prediction$data[i,3] <= 0.20) & (minnotest[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*minnotest[i,141])
        BR = BR + ((stake*minnotest[i,141]) - stake)
        nb = nb + 1
      }
      if((prediction$data[i,3] > 0.80) & (minnotest[i,141] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    } else{
      if((prediction$data[i,3] >= 0.80) & (minnotest[i,141] > 0)){
        bet = bet + stake
        win = win + (stake*minnotest[i,141])
        BR = BR + ((stake*minnotest[i,141]) - stake)
        nb = nb + 1
      }
      if((prediction$data[i,3] < 0.20) & (minnotest[i,141] > 0)){
        bet = bet + stake
        BR = BR - stake
        nb = nb + 1
      }
    }
    if (BR <=0){
      break
    }  
  }
  
  
  win = win - bet
  
  ret = win/bet
  
  print(ret)

}

hpbcfil()




#### repeat all above for SVM's


# Recreate predict value for SVM Full Data Model

set.seed(41)
learner = makeLearner("classif.svm", predict.type = "prob", cost = 4, nu = 8, kernel = "polynomial")

task = makeClassifTask(data = SMLtrain1, target ="AS_win_lose_binary")

train.set = c(1:nrow(SMLtrain1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = SMLtest1)


# betting and money management functions from above

eprbsf()

mprbsf()

eprkcf()

mprkcf()

eprrkcf()

mprrkcf()

eprbcf()

mprbcf()

favbcf()

hpbcf()


# Recreate predict value for SVM Filtered Data Model

set.seed(43)
learner = makeLearner("classif.svm", predict.type = "prob", cost = 1, nu = 0, kernel = "linear")

task = makeClassifTask(data = SMLmtr1, target ="AS_win_lose_binary")

train.set = c(1:nrow(SMLmtr1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = SMLmte1)


# betting and money management functions from above

eprbsfil()

mprbsfil()

eprkcfil()

mprkcfil()

eprrkcfil()

mprrkcfil()

eprbcfil()

mprbcfil()

favbcfil()

hpbcfil()


# betting stategies and MM against filtered data glm models

#re-create 'pre' value for filtered model 

pre = predict(glm3, LRtest, type = "response")


#BS1  bet if expected postive return

bet = 0
win = 0

for (i in 1:438){
  if((minnotest[i,185])==0){
    if((pre[i]*minnotest[i,142]) > 1){
      bet = bet + 1
    } 
    if(((1- pre[i])*minnotest[i,141]) >1){
      bet = bet + 1
      win = win + minnotest[i,141]
    }
  } else{
    if((pre[i]*minnotest[i,141]) > 1){
      win = win + minnotest[i,141]
      bet = bet + 1
    } 
    if(((1- pre[i])*minnotest[i,142]) >1){
      bet = bet + 1
    }
  }
}

i

win = win - bet

ret = win/bet

ret


#BS2 bet model prob - bookmaker prob >= 0.05

bet = 0
win = 0

for (i in 1:438){
  if((minnotest[i,185])==0){
    if(((pre[i] - (1/minnotest[i,142])) >=  0.05) & (minnotest[i,142] > 0)){
      bet = bet + 1
    }
    if((((1-pre[i]) - (1/minnotest[i,141])) >= 0.05) & (minnotest[i,141] > 0)){
      bet = bet + 1
      win = win + minnotest[i,141]
    }
  } else{
    if(((pre[i] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,141] > 0)){
      win = win + minnotest[i,141]
      bet = bet + 1
    }
    if((((1-pre[i]) - (1/minnotest[i,142])) >= 0.05) & (minnotest[i,142] > 0)){
      bet = bet + 1
    } 
  }
}



win = win - bet

ret = win/bet

ret


#MM Strategy #1 - BS#1 with Kelly Criterion

bet = 0
win = 0

for (i in 1:438){
  stake1 = ((pre[i]*minnotest[i,142])-1)/(minnotest[i,142]-1)
  stake3 = ((pre[i]*minnotest[i,141])-1)/(minnotest[i,141]-1)
  stake2 = (((1-pre[i])*minnotest[i,141])-1)/(minnotest[i,141]-1)
  stake4 = (((1-pre[i])*minnotest[i,142])-1)/(minnotest[i,142]-1)
  if((minnotest[i,185])==0){
    if((pre[i]*minnotest[i,142]) > 1){
      bet = bet + stake1
    } 
    if(((1- pre[i])*minnotest[i,141]) >1){
      bet = bet + stake2
      win = win + (stake2*minnotest[i,141]) 
    }
  } else{
    if((pre[i]*minnotest[i,141]) > 1){
      win = win + (stake3*minnotest[i,141])
      bet = bet + stake3
    } 
    if(((1- pre[i])*minnotest[i,142]) >1){
      bet = bet + stake4
    }
  }
}

win = win - bet

ret = win/bet

ret


#MM Strategy #1 - BS#2 with Kelly Criterion

bet = 0
win = 0

for (i in 1:438){
  stake1 = ((pre[i]*minnotest[i,142])-1)/(minnotest[i,142]-1)
  stake3 = ((pre[i]*minnotest[i,141])-1)/(minnotest[i,141]-1)
  stake2 = (((1-pre[i])*minnotest[i,141])-1)/(minnotest[i,141]-1)
  stake4 = (((1-pre[i])*minnotest[i,142])-1)/(minnotest[i,142]-1)
  if((minnotest[i,185])==0){
    if(((pre[i] - (1/minnotest[i,142])) >=  0.05) & (minnotest[i,142] > 0)){
      bet = bet + stake1
    } 
    if((((1-pre[i]) - (1/minnotest[i,141])) >= 0.05) & (minnotest[i,141] > 0)){
      bet = bet + stake2
      win = win + (stake2*minnotest[i,141]) 
    }
  } else{
    if(((pre[i] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,141] > 0)){
      win = win + (stake3*minnotest[i,141])
      bet = bet + stake3
    } 
    if((((1-pre[i]) - (1/minnotest[i,142])) >= 0.05) & (minnotest[i,142] > 0)){
      bet = bet + stake4
    }
  }
}

win = win - bet

ret = win/bet

ret


#MM Strategy #2 - BS#1 with Kelly Criterion reversed

bet = 0
win = 0

for (i in 1:438){
  stake1 = 1 - ((pre[i]*minnotest[i,142])-1)/(minnotest[i,142]-1)
  stake3 = 1 - ((pre[i]*minnotest[i,141])-1)/(minnotest[i,141]-1)
  stake2 = 1 - (((1-pre[i])*minnotest[i,141])-1)/(minnotest[i,141]-1)
  stake4 = 1 - (((1-pre[i])*minnotest[i,142])-1)/(minnotest[i,142]-1)
  if((minnotest[i,185])==0){
    if((pre[i]*minnotest[i,142]) > 1){
      bet = bet + stake1
    } 
    if(((1- pre[i])*minnotest[i,141]) >1){
      bet = bet + stake2
      win = win + (stake2*minnotest[i,141]) 
    }
  } else{
    if((pre[i]*minnotest[i,141]) > 1){
      win = win + (stake3*minnotest[i,141])
      bet = bet + stake3
    } 
    if(((1- pre[i])*minnotest[i,142]) >1){
      bet = bet + stake4
    }
  }
}

win = win - bet

ret = win/bet

ret


#MM Strategy #2 - BS#2 with Kelly Criterion reversed

bet = 0
win = 0

for (i in 1:438){
  stake1 = 1 - ((pre[i]*minnotest[i,142])-1)/(minnotest[i,142]-1)
  stake3 = 1 - ((pre[i]*minnotest[i,141])-1)/(minnotest[i,141]-1)
  stake2 = 1 - (((1-pre[i])*minnotest[i,141])-1)/(minnotest[i,141]-1)
  stake4 = 1 - (((1-pre[i])*minnotest[i,142])-1)/(minnotest[i,142]-1)
  if((minnotest[i,185])==0){
    if(((pre[i] - (1/minnotest[i,142])) >=  0.05) & (minnotest[i,142] > 0)){
      bet = bet + stake1
    } 
    if((((1-pre[i]) - (1/minnotest[i,141])) >= 0.05) & (minnotest[i,141] > 0)){
      bet = bet + stake2
      win = win + (stake2*minnotest[i,141]) 
    }
  } else{
    if(((pre[i] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,141] > 0)){
      win = win + (stake3*minnotest[i,141])
      bet = bet + stake3
    } 
    if((((1-pre[i]) - (1/minnotest[i,142])) >= 0.05) & (minnotest[i,142] > 0)){
      bet = bet + stake4
    }
  }
}

win = win - bet

ret = win/bet

ret


#MM Strategy #3 - BS#1 with % of Bankroll Criterion

nb = 0
bet = 0
win = 0
BR = 438

for (i in 1:438){
  stake = 0.01*BR
  if((minnotest[i,185])==0){
    if((pre[i]*minnotest[i,142]) > 1){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    } 
    if(((1- pre[i])*minnotest[i,141]) >1){
      bet = bet + stake
      win = win + (stake*minnotest[i,141])
      BR = BR + ((stake*minnotest[i,141]) - stake)
      nb = nb + 1
    }
  } else{
    if((pre[i]*minnotest[i,141]) > 1){
      win = win + (stake*minnotest[i,141])
      bet = bet + stake
      BR = BR + ((stake*minnotest[i,141]) - stake)
      nb = nb + 1
    } 
    if(((1- pre[i])*minnotest[i,142]) >1){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  }
  if (BR <=0){
    break
  }
}



win = win - bet

ret = win/bet

ret


#MM Strategy #3 - BS#2 with % of Bankroll Criterion


nb = 0
bet = 0
win = 0
BR = 438

for (i in 1:438){
  stake = 0.01*BR
  if((minnotest[i,185])==0){
    if(((pre[i] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,142] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    } 
    if((((1-pre[i]) - (1/minnotest[i,141])) >= 0.05) & (minnotest[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*minnotest[i,141])
      BR = BR + ((stake*minnotest[i,141]) - stake)
      nb = nb + 1
    }
  } else{
    if(((pre[i] - (1/minnotest[i,141])) >=  0.05) & (minnotest[i,141] > 0)){
      win = win + (stake*minnotest[i,141])
      bet = bet + stake
      BR = BR + ((stake*minnotest[i,141]) - stake)
      nb = nb + 1
    } 
    if((((1-pre[i]) - (1/minnotest[i,142])) >= 0.05) & (minnotest[i,142] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  }
  if (BR <=0){
    break
  }
}



win = win - bet

ret = win/bet

ret


#MM Strategy #3 - bet on favourite betting staregy in first part of project with % of Bankroll Criterion

nb = 0
bet = 0
win = 0
BR = 438


for (i in 1:438){
  stake = 0.01*BR
  if((minnotest[i, 185])==0){
    if((pre[i] <= 0.50) & (minnotest[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*minnotest[i,141])
      BR = BR + ((stake*minnotest[i,141]) - stake)
      nb = nb + 1
    }
    if((pre[i] > 0.50) & (minnotest[i,141] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  } else{
    if((pre[i] >= 0.50) & (minnotest[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*minnotest[i,141])
      BR = BR + ((stake*minnotest[i,141]) - stake)
      nb = nb + 1
    }
    if((pre[i] < 0.50) & (minnotest[i,141] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  }
  if (BR <=0){
    break
  }
}


win = win - bet

ret = win/bet

ret


#MM Strategy #3 - bet on high probabilities betting staregy in first part project with % of Bankroll Criterion

nb = 0
bet = 0
win = 0
BR = 438

for (i in 1:438){
  stake = 0.01*BR
  if((minnotest[i, 185])==0){
    if((pre[i] <= 0.20) & (minnotest[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*minnotest[i,141])
      BR = BR + ((stake*minnotest[i,141]) - stake)
      nb = nb + 1
    }
    if((pre[i] > 0.80) & (minnotest[i,141] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  } else{
    if((pre[i] >= 0.80) & (minnotest[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*minnotest[i,141])
      BR = BR + ((stake*minnotest[i,141]) - stake)
      nb = nb + 1
    }
    if((pre[i] < 0.20) & (minnotest[i,141] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  }
  if (BR <=0){
    break
  }  
}


win = win - bet

ret = win/bet

ret


### betting strategies and MM for full data GLM models

#re-create 'pre' value for full data model 

pre = predict(glm6, minnotest, type = "response")

#BS1 bet if expected positive return 

bet = 0
win = 0


for (i in 1:2282){
  if((tenniste[i,185])==0){
    if((pre[i]*tenniste[i,142]) > 1){
      bet = bet + 1
    } 
    if(((1- pre[i])*tenniste[i,141]) >1){
      bet = bet + 1
      win = win + tenniste[i,141]
    }
  } else{
    if((pre[i]*tenniste[i,141]) > 1){
      win = win + tenniste[i,141]
      bet = bet + 1
    } 
    if(((1- pre[i])*tenniste[i,142]) >1){
      bet = bet + 1
    }
  }
}



win = win - bet

ret = win/bet

ret


#BS2 bet model prob - bookmaker prob >= 0.05

bet = 0
win = 0

for (i in 1:2282){
  if((tenniste[i,185])==0){
    if(((pre[i] - (1/tenniste[i,142])) >=  0.05) & (tenniste[i,142] > 0)){
      bet = bet + 1
    }
    if((((1-pre[i]) - (1/tenniste[i,141])) >= 0.05) & (tenniste[i,141] > 0)){
      bet = bet + 1
      win = win + tenniste[i,141]
    }
  } else{
    if(((pre[i] - (1/tenniste[i,141])) >=  0.05) & (tenniste[i,141] > 0)){
      win = win + tenniste[i,141]
      bet = bet + 1
    }
    if((((1-pre[i]) - (1/tenniste[i,142])) >= 0.05) & (tenniste[i,142] > 0)){
      bet = bet + 1
    } 
  }
}



win = win - bet

ret = win/bet

ret


#MM Strategy #1 - BS#1 with Kelly Criterion

bet = 0
win = 0

for (i in 1:2282){
  stake1 = ((pre[i]*tenniste[i,142])-1)/(tenniste[i,142]-1)
  stake3 = ((pre[i]*tenniste[i,141])-1)/(tenniste[i,141]-1)
  stake2 = (((1-pre[i])*tenniste[i,141])-1)/(tenniste[i,141]-1)
  stake4 = (((1-pre[i])*tenniste[i,142])-1)/(tenniste[i,142]-1)
  if((tenniste[i,185])==0){
    if((pre[i]*tenniste[i,142]) > 1){
      bet = bet + stake1
    } 
    if(((1- pre[i])*tenniste[i,141]) >1){
      bet = bet + stake2
      win = win + (stake2*tenniste[i,141]) 
    }
  } else{
    if((pre[i]*tenniste[i,141]) > 1){
      win = win + (stake3*tenniste[i,141])
      bet = bet + stake3
    } 
    if(((1- pre[i])*tenniste[i,142]) >1){
      bet = bet + stake4
    }
  }
}


win = win - bet

ret = win/bet

ret


#MM Strategy #1 - BS#2 with Kelly Criterion

bet = 0
win = 0

for (i in 1:2282){
  stake1 = ((pre[i]*tenniste[i,142])-1)/(tenniste[i,142]-1)
  stake3 = ((pre[i]*tenniste[i,141])-1)/(tenniste[i,141]-1)
  stake2 = (((1-pre[i])*tenniste[i,141])-1)/(tenniste[i,141]-1)
  stake4 = (((1-pre[i])*tenniste[i,142])-1)/(tenniste[i,142]-1)
  if((tenniste[i,185])==0){
    if(((pre[i] - (1/tenniste[i,142])) >=  0.05) & (tenniste[i,142] > 0)){
      bet = bet + stake1
    } 
    if((((1-pre[i]) - (1/tenniste[i,141])) >= 0.05) & (tenniste[i,141] > 0)){
      bet = bet + stake2
      win = win + (stake2*tenniste[i,141]) 
    }
  } else{
    if(((pre[i] - (1/tenniste[i,141])) >=  0.05) & (tenniste[i,141] > 0)){
      win = win + (stake3*tenniste[i,141])
      bet = bet + stake3
    } 
    if((((1-pre[i]) - (1/tenniste[i,142])) >= 0.05) & (tenniste[i,142] > 0)){
      bet = bet + stake4
    }
  }
}

win = win - bet

ret = win/bet

ret


#MM Strategy #2 - BS#1 with Kelly Criterion reversed

bet = 0
win = 0

for (i in 1:2282){
  stake1 = 1- ((pre[i]*tenniste[i,142])-1)/(tenniste[i,142]-1)
  stake3 = 1 - ((pre[i]*tenniste[i,141])-1)/(tenniste[i,141]-1)
  stake2 = 1 - (((1-pre[i])*tenniste[i,141])-1)/(tenniste[i,141]-1)
  stake4 = 1 - (((1-pre[i])*tenniste[i,142])-1)/(tenniste[i,142]-1)
  if((tenniste[i,185])==0){
    if((pre[i]*tenniste[i,142]) > 1){
      bet = bet + stake1
    } 
    if(((1- pre[i])*tenniste[i,141]) >1){
      bet = bet + stake2
      win = win + (stake2*tenniste[i,141]) 
    }
  } else{
    if((pre[i]*tenniste[i,141]) > 1){
      win = win + (stake3*tenniste[i,141])
      bet = bet + stake3
    } 
    if(((1- pre[i])*tenniste[i,142]) >1){
      bet = bet + stake4
    }
  }
}

win = win - bet

ret = win/bet

ret


#MM Strategy #2 - BS#2 with Kelly Criterion reversed

bet = 0
win = 0

for (i in 1:2282){
  stake1 = 1 - ((pre[i]*tenniste[i,142])-1)/(tenniste[i,142]-1)
  stake3 = 1 - ((pre[i]*tenniste[i,141])-1)/(tenniste[i,141]-1)
  stake2 = 1 - (((1-pre[i])*tenniste[i,141])-1)/(tenniste[i,141]-1)
  stake4 = 1 - (((1-pre[i])*tenniste[i,142])-1)/(tenniste[i,142]-1)
  if((tenniste[i,185])==0){
    if(((pre[i] - (1/tenniste[i,142])) >=  0.05) & (tenniste[i,142] > 0)){
      bet = bet + stake1
    } 
    if((((1-pre[i]) - (1/tenniste[i,141])) >= 0.05) & (tenniste[i,141] > 0)){
      bet = bet + stake2
      win = win + (stake2*tenniste[i,141]) 
    }
  } else{
    if(((pre[i] - (1/tenniste[i,141])) >=  0.05) & (tenniste[i,141] > 0)){
      win = win + (stake3*tenniste[i,141])
      bet = bet + stake3
    } 
    if((((1-pre[i]) - (1/tenniste[i,142])) >= 0.05) & (tenniste[i,142] > 0)){
      bet = bet + stake4
    }
  }
}

win = win - bet

ret = win/bet

ret


#MM Strategy #3 - BB#1 with % of Bankroll Criterion

nb = 0
bet = 0
win = 0
BR = 2282

for (i in 1:2282){
  stake = 0.01*BR
  if((tenniste[i,185])==0){
    if((pre[i]*tenniste[i,142]) > 1){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    } 
    if(((1- pre[i])*tenniste[i,141]) >1){
      bet = bet + stake
      win = win + (stake*tenniste[i,141])
      BR = BR + ((stake*tenniste[i,141]) - stake)
      nb = nb + 1
    }
  } else{
    if((pre[i]*tenniste[i,141]) > 1){
      win = win + (stake*tenniste[i,141])
      bet = bet + stake
      BR = BR + ((stake*tenniste[i,141]) - stake)
      nb = nb + 1
    } 
    if(((1- pre[i])*tenniste[i,142]) >1){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  }
  if (BR <=0){
    break
  }
}



win = win - bet

ret = win/bet

ret


#MM Strategy #3 - BS#2 with % of Bankroll Criterion


nb = 0
bet = 0
win = 0
BR = 2282

for (i in 1:2282){
  stake = 0.01*BR
  if((tenniste[i,185])==0){
    if(((pre[i] - (1/tenniste[i,142])) >=  0.05) & (tenniste[i,142] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    } 
    if((((1-pre[i]) - (1/tenniste[i,141])) >= 0.05) & (tenniste[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*tenniste[i,141])
      BR = BR + ((stake*tenniste[i,141]) - stake)
      nb = nb + 1
    }
  } else{
    if(((pre[i] - (1/tenniste[i,141])) >=  0.05) & (tenniste[i,141] > 0)){
      win = win + (stake*tenniste[i,141])
      bet = bet + stake
      BR = BR + ((stake*tenniste[i,141]) - stake)
      nb = nb + 1
    } 
    if((((1-pre[i]) - (1/tenniste[i,142])) >= 0.05) & (tenniste[i,142] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  }
  if (BR <=0){
    break
  }
}



win = win - bet

ret = win/bet

ret


#MM Strategy #2 - bet on favourite betting staregy in first part of project with % of Bankroll Criterion

nb = 0
bet = 0
win = 0
BR = 2282


for (i in 1:2282){
  stake = 0.01*BR
  if((tenniste[i, 185])==0){
    if((pre[i] <= 0.50) & (tenniste[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*tenniste[i,141])
      BR = BR + ((stake*tenniste[i,141]) - stake)
      nb = nb + 1
    }
    if((pre[i] > 0.50) & (tenniste[i,141] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  } else{
    if((pre[i] >= 0.50) & (tenniste[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*tenniste[i,141])
      BR = BR + ((stake*tenniste[i,141]) - stake)
      nb = nb + 1
    }
    if((pre[i] < 0.50) & (tenniste[i,141] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  }
  if (BR <=0){
    break
  }
}


win = win - bet

ret = win/bet

ret


#MM Strategy #2 - bet on high probabilities betting staregy in first part of project with % of Bankroll Criterion

nb = 0
bet = 0
win = 0
BR = 2282

for (i in 1:2282){
  stake = 0.01*BR
  if((tenniste[i, 185])==0){
    if((pre[i] <= 0.20) & (tenniste[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*tenniste[i,141])
      BR = BR + ((stake*tenniste[i,141]) - stake)
      nb = nb + 1
    }
    if((pre[i] > 0.80) & (tenniste[i,141] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  } else{
    if((pre[i] >= 0.80) & (tenniste[i,141] > 0)){
      bet = bet + stake
      win = win + (stake*tenniste[i,141])
      BR = BR + ((stake*tenniste[i,141]) - stake)
      nb = nb + 1
    }
    if((pre[i] < 0.20) & (tenniste[i,141] > 0)){
      bet = bet + stake
      BR = BR - stake
      nb = nb + 1
    }
  }
  if (BR <=0){
    break
  }  
}


win = win - bet

ret = win/bet

ret


## statistical significance tests of differences between model means and data set means 

RFFull = c(-7.1, -7.1, -6.3, -6.7, -3.2, -3.4, -6.7, -2.6, -2.2, -2.9, -5.5, -1.2)
LRFull = c(-6.2, -6.9, -2.3, -4.2, -5.6, -4.5, -3.7, -6.1, -6.6, -5.9, -3.7, -7.7)
SVMFull = c(-2, -2.3, .7, .3, -1.5, -1.9, -.3, -1.8, -1.4, -2, 0, -1.8)
ANNFull = c(-5.6, -5.4, .2, -1.5, -3.5, -3.2, -3.1, -3.8, -4.3, -4.3,-3.2, -5.3)

RFFil = c(-5.2, -5.2, 1.5, 1.3, 1.1, 0, -.7, 1.5, 6.3, 6, 1.3, 8)
LRFil = c(-2.6, -3.2, -2.3, -2.4, -3.5, -4.2, -3.4, -3.7, -1.5, .6, -3.3, 2.7)
SVMFil = c(-8.7, -8.6, -.3, -.4, .8, -.4, 1.9, .6, 9.4, 7.2, 6.1, 10.2)
ANNFil = c(-3.3, -5, 1, -.5, -.4, -1.7, .9, -1.4, -.5, -.5, 1, -1.9)

mean(RFFull)
mean(LRFull)
mean(SVMFull)
mean(ANNFull)
mean(RFFil)
mean(LRFil)
mean(SVMFil)
mean(ANNFil)

AMFull = c(-7.1, -7.1, -6.3, -6.7, -3.2, -3.4, -6.7, -2.6, -2.2, -2.9, -5.5, -1.2,-6.2, -6.9, -2.3, -4.2, -5.6, -4.5, -3.7, -6.1, -6.6, -5.9, -3.7, -7.7, -2, -2.3, .7, .3, -1.5, -1.9, -.3, -1.8, -1.4, -2, 0, -1.8,-5.6, -5.4, .2, -1.5, -3.5, -3.2, -3.1, -3.8, -4.3, -4.3,-3.2, -5.3)

AMFil = c(-5.2, -5.2, 1.5, 1.3, 1.1, 0, -.7, 1.5, 6.3, 6, 1.3, 8,-2.6, -3.2, -2.3, -2.4, -3.5, -4.2, -3.4, -3.7, -1.5, .6, -3.3, 2.7, -8.7, -8.6, -.3, -.4, .8, -.4, 1.9, .6, 9.4, 7.2, 6.1, 10.2, -3.3, -5, 1, -.5, -.4, -1.7, .9, -1.4, -.5, -.5, 1, -1.9)

mean(AMFull)
mean(AMFil)

RFC = c(-7.1, -7.1, -6.3, -6.7, -3.2, -3.4, -6.7, -2.6, -2.2, -2.9, -5.5, -1.2, -5.2, -5.2, 1.5, 1.3, 1.1, 0, -.7, 1.5, 6.3, 6, 1.3, 8)
LRC = c(-6.2, -6.9, -2.3, -4.2, -5.6, -4.5, -3.7, -6.1, -6.6, -5.9, -3.7, -7.7, -2.6, -3.2, -2.3, -2.4, -3.5, -4.2, -3.4, -3.7, -1.5, .6, -3.3, 2.7)
SVMC = c(-2, -2.3, .7, .3, -1.5, -1.9, -.3, -1.8, -1.4, -2, 0, -1.8, -8.7, -8.6, -.3, -.4, .8, -.4, 1.9, .6, 9.4, 7.2, 6.1, 10.2)
ANNC = c(-5.6, -5.4, .2, -1.5, -3.5, -3.2, -3.1, -3.8, -4.3, -4.3,-3.2, -5.3, -3.3, -5, 1, -.5, -.4, -1.7, .9, -1.4, -.5, -.5, 1, -1.9)



t.test(AMFull, AMFil,alternative = "two.sided", var.equal = FALSE)

t.test(SVMFull, LRFull,alternative = "two.sided", var.equal = FALSE)
t.test(SVMFull, RFFull,alternative = "two.sided", var.equal = FALSE)
t.test(SVMFull, ANNFull,alternative = "two.sided", var.equal = FALSE)

t.test(SVMFil, LRFil,alternative = "two.sided", var.equal = FALSE)
t.test(SVMFil, RFFil,alternative = "two.sided", var.equal = FALSE)
t.test(SVMFil, ANNFil,alternative = "two.sided", var.equal = FALSE)

t.test(SVMC, LRC,alternative = "two.sided", var.equal = FALSE)
t.test(SVMC, RFC,alternative = "two.sided", var.equal = FALSE)
t.test(SVMC, ANNC,alternative = "two.sided", var.equal = FALSE)


## get numbers to test random betting & Money Management Strategy

set.seed(45)

r1 = sample(1:15,1)

r1

set.seed(46)

r2 = sample(1:3,1)

r2


###  reinforcement learning data prep 

# Create predict values for Random Forest Full Data Model 


learner = makeLearner("classif.randomForest", ntree = 200, mtry = 4, predict.type = "prob")

task = makeClassifTask(data = MLtrain1, target ="AS_win_lose_binary")

train.set = c(1:nrow(MLtrain1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = MLtest)


prob = c()
w_l = c()
b=1

for (i in 1:2282){
  if ((tenniste[i,183] >=1.95) & (tenniste[i,183] <= 2.05)){
    prob[b] = prediction$data[i,3]
    w_l[b] = tenniste[i,185]
    b = b + 1 
  }
}



path = "D:\\RLprobs.csv"
write.csv(prob, path)

path = "D:\\RLw_l.csv"
write.csv(w_l, path)





## plot one set of bankroll movements (extracted from google colab output)

Bankroll = c(75,76,74,73,74,73,75,74,73,72,73,74,73,74,75,76,77,75,74,75,76,77,75,76,75,76,75,74,75,76,75,74,73,72,75,76,75,74,73,74,73,71,70,90,89,78,100,101)

length(Bankroll)

plot(Bankroll, ylab = "BANKROLL", xlab = "MATCHES", pch=30, yaxt ="n", xaxt = "n")
axis(2, cex.axis =2)
axis(1, cex.axis =2)
lines(Bankroll, col =2)


### PCA comparison test

SMLtrain2 = SMLtrain1[,-29]

traincor = cor(SMLtrain2)

pcatrain = prcomp(traincor, scale = TRUE)
pcatrain 
summary(pcatrain)


plot(pcatrain$sdev^2, xlab = "Component number", ylab = "Component variance", type
     = "l", main = "Scree diagram");

trainb = SMLtrain2
trainb1 = trainb


for (i in 1:5325){
  trainb1[i,1] = trainb[i,1]*pcatrain$rotation[1] + trainb[i,2]*pcatrain$rotation[2] +trainb[i,3]*pcatrain$rotation[3] +trainb[i,4]*pcatrain$rotation[4] +trainb[i,5]*pcatrain$rotation[5] +trainb[i,6]*pcatrain$rotation[6] +trainb[i,7]*pcatrain$rotation[7] +trainb[i,8]*pcatrain$rotation[8]+trainb[i,9]*pcatrain$rotation[9] +trainb[i,10]*pcatrain$rotation[10] +trainb[i,11]*pcatrain$rotation[11] +trainb[i,12]*pcatrain$rotation[12] +trainb[i,13]*pcatrain$rotation[13]   
  + trainb[i,14]*pcatrain$rotation[14] + trainb[i,15]*pcatrain$rotation[15] +trainb[i,16]*pcatrain$rotation[16] +trainb[i,17]*pcatrain$rotation[17] +trainb[i,18]*pcatrain$rotation[18] +trainb[i,19]*pcatrain$rotation[19] +trainb[i,20]*pcatrain$rotation[20] +trainb[i,21]*pcatrain$rotation[21]+trainb[i,22]*pcatrain$rotation[22] +trainb[i,23]*pcatrain$rotation[23] +trainb[i,24]*pcatrain$rotation[24] +trainb[i,25]*pcatrain$rotation[25] +trainb[i,26]*pcatrain$rotation[26]    
  + trainb[i,27]*pcatrain$rotation[27] + trainb[i,28]*pcatrain$rotation[28] +trainb[i,29]*pcatrain$rotation[29]
  trainb1[i,2] = trainb[i,1]*pcatrain$rotation[30] + trainb[i,2]*pcatrain$rotation[31] +trainb[i,3]*pcatrain$rotation[32] +trainb[i,4]*pcatrain$rotation[33] +trainb[i,5]*pcatrain$rotation[34] +trainb[i,6]*pcatrain$rotation[35] +trainb[i,7]*pcatrain$rotation[36] +trainb[i,8]*pcatrain$rotation[37]+trainb[i,9]*pcatrain$rotation[38] +trainb[i,10]*pcatrain$rotation[39] +trainb[i,11]*pcatrain$rotation[40] +trainb[i,12]*pcatrain$rotation[41] +trainb[i,13]*pcatrain$rotation[42]   
  + trainb[i,14]*pcatrain$rotation[43] + trainb[i,15]*pcatrain$rotation[44] +trainb[i,16]*pcatrain$rotation[45] +trainb[i,17]*pcatrain$rotation[46] +trainb[i,18]*pcatrain$rotation[47] +trainb[i,19]*pcatrain$rotation[48] +trainb[i,20]*pcatrain$rotation[49] +trainb[i,21]*pcatrain$rotation[50]+trainb[i,22]*pcatrain$rotation[51] +trainb[i,23]*pcatrain$rotation[52] +trainb[i,24]*pcatrain$rotation[53] +trainb[i,25]*pcatrain$rotation[54] +trainb[i,26]*pcatrain$rotation[55]    
  + trainb[i,27]*pcatrain$rotation[56] + trainb[i,28]*pcatrain$rotation[57] +trainb[i,29]*pcatrain$rotation[58]
  trainb1[i,3] = trainb[i,1]*pcatrain$rotation[59] + trainb[i,2]*pcatrain$rotation[60] +trainb[i,3]*pcatrain$rotation[61] +trainb[i,4]*pcatrain$rotation[62] +trainb[i,5]*pcatrain$rotation[63] +trainb[i,6]*pcatrain$rotation[64] +trainb[i,7]*pcatrain$rotation[65] +trainb[i,8]*pcatrain$rotation[66]+trainb[i,9]*pcatrain$rotation[67] +trainb[i,10]*pcatrain$rotation[68] +trainb[i,11]*pcatrain$rotation[69] +trainb[i,12]*pcatrain$rotation[70] +trainb[i,13]*pcatrain$rotation[71]   
  + trainb[i,14]*pcatrain$rotation[72] + trainb[i,15]*pcatrain$rotation[73] +trainb[i,16]*pcatrain$rotation[74] +trainb[i,17]*pcatrain$rotation[75] +trainb[i,18]*pcatrain$rotation[76] +trainb[i,19]*pcatrain$rotation[77] +trainb[i,20]*pcatrain$rotation[78] +trainb[i,21]*pcatrain$rotation[79]+trainb[i,22]*pcatrain$rotation[80] +trainb[i,23]*pcatrain$rotation[81] +trainb[i,24]*pcatrain$rotation[82] +trainb[i,25]*pcatrain$rotation[83] +trainb[i,26]*pcatrain$rotation[84]    
  + trainb[i,27]*pcatrain$rotation[85] + trainb[i,28]*pcatrain$rotation[86] +trainb[i,29]*pcatrain$rotation[87]
  trainb1[i,4] = trainb[i,1]*pcatrain$rotation[88] + trainb[i,2]*pcatrain$rotation[89] +trainb[i,3]*pcatrain$rotation[90] +trainb[i,4]*pcatrain$rotation[91] +trainb[i,5]*pcatrain$rotation[92] +trainb[i,6]*pcatrain$rotation[93] +trainb[i,7]*pcatrain$rotation[94] +trainb[i,8]*pcatrain$rotation[95]+trainb[i,9]*pcatrain$rotation[96] +trainb[i,10]*pcatrain$rotation[97] +trainb[i,11]*pcatrain$rotation[98] +trainb[i,12]*pcatrain$rotation[99] +trainb[i,13]*pcatrain$rotation[100]   
  + trainb[i,14]*pcatrain$rotation[101] + trainb[i,15]*pcatrain$rotation[102] +trainb[i,16]*pcatrain$rotation[103] +trainb[i,17]*pcatrain$rotation[104] +trainb[i,18]*pcatrain$rotation[105] +trainb[i,19]*pcatrain$rotation[106] +trainb[i,20]*pcatrain$rotation[107] +trainb[i,21]*pcatrain$rotation[108]+trainb[i,22]*pcatrain$rotation[109] +trainb[i,23]*pcatrain$rotation[110] +trainb[i,24]*pcatrain$rotation[111] +trainb[i,25]*pcatrain$rotation[112] +trainb[i,26]*pcatrain$rotation[113]    
  + trainb[i,27]*pcatrain$rotation[114] + trainb[i,28]*pcatrain$rotation[115] +trainb[i,29]*pcatrain$rotation[116]
  trainb1[i,5] = trainb[i,1]*pcatrain$rotation[117] + trainb[i,2]*pcatrain$rotation[118] +trainb[i,3]*pcatrain$rotation[119] +trainb[i,4]*pcatrain$rotation[120] +trainb[i,5]*pcatrain$rotation[121] +trainb[i,6]*pcatrain$rotation[122] +trainb[i,7]*pcatrain$rotation[123] +trainb[i,8]*pcatrain$rotation[124]+trainb[i,9]*pcatrain$rotation[125] +trainb[i,10]*pcatrain$rotation[126] +trainb[i,11]*pcatrain$rotation[127] +trainb[i,12]*pcatrain$rotation[128] +trainb[i,13]*pcatrain$rotation[129]   
  + trainb[i,14]*pcatrain$rotation[130] + trainb[i,15]*pcatrain$rotation[131] +trainb[i,16]*pcatrain$rotation[132] +trainb[i,17]*pcatrain$rotation[133] +trainb[i,18]*pcatrain$rotation[134] +trainb[i,19]*pcatrain$rotation[135] +trainb[i,20]*pcatrain$rotation[136] +trainb[i,21]*pcatrain$rotation[137]+trainb[i,22]*pcatrain$rotation[138] +trainb[i,23]*pcatrain$rotation[139] +trainb[i,24]*pcatrain$rotation[140] +trainb[i,25]*pcatrain$rotation[141] +trainb[i,26]*pcatrain$rotation[142]    
  + trainb[i,27]*pcatrain$rotation[143] + trainb[i,28]*pcatrain$rotation[144] +trainb[i,29]*pcatrain$rotation[145]
  trainb1[i,6] = trainb[i,1]*pcatrain$rotation[146] + trainb[i,2]*pcatrain$rotation[147] +trainb[i,3]*pcatrain$rotation[148] +trainb[i,4]*pcatrain$rotation[149] +trainb[i,5]*pcatrain$rotation[150] +trainb[i,6]*pcatrain$rotation[151] +trainb[i,7]*pcatrain$rotation[152] +trainb[i,8]*pcatrain$rotation[153]+trainb[i,9]*pcatrain$rotation[154] +trainb[i,10]*pcatrain$rotation[155] +trainb[i,11]*pcatrain$rotation[156] +trainb[i,12]*pcatrain$rotation[157] +trainb[i,13]*pcatrain$rotation[158]   
  + trainb[i,14]*pcatrain$rotation[159] + trainb[i,15]*pcatrain$rotation[160] +trainb[i,16]*pcatrain$rotation[161] +trainb[i,17]*pcatrain$rotation[162] +trainb[i,18]*pcatrain$rotation[163] +trainb[i,19]*pcatrain$rotation[164] +trainb[i,20]*pcatrain$rotation[165] +trainb[i,21]*pcatrain$rotation[166]+trainb[i,22]*pcatrain$rotation[167] +trainb[i,23]*pcatrain$rotation[168] +trainb[i,24]*pcatrain$rotation[169] +trainb[i,25]*pcatrain$rotation[170] +trainb[i,26]*pcatrain$rotation[171]    
  + trainb[i,27]*pcatrain$rotation[172] + trainb[i,28]*pcatrain$rotation[173] +trainb[i,29]*pcatrain$rotation[174]
}


trainb1 = trainb1[ ,-c(7:29)]
trainb1[,7] = SMLtrain1[,29]


SMLtest2 = SMLtest1[,-29]

testb = SMLtest2
testb1 = testb

for (i in 1:2282){
  testb1[i,1] = testb[i,1]*pcatrain$rotation[1] + testb[i,2]*pcatrain$rotation[2] +testb[i,3]*pcatrain$rotation[3] +testb[i,4]*pcatrain$rotation[4] +testb[i,5]*pcatrain$rotation[5] +testb[i,6]*pcatrain$rotation[6] +testb[i,7]*pcatrain$rotation[7] +testb[i,8]*pcatrain$rotation[8]+testb[i,9]*pcatrain$rotation[9] +testb[i,10]*pcatrain$rotation[10] +testb[i,11]*pcatrain$rotation[11] +testb[i,12]*pcatrain$rotation[12] +testb[i,13]*pcatrain$rotation[13]   
  + testb[i,14]*pcatrain$rotation[14] + testb[i,15]*pcatrain$rotation[15] +testb[i,16]*pcatrain$rotation[16] +testb[i,17]*pcatrain$rotation[17] +testb[i,18]*pcatrain$rotation[18] +testb[i,19]*pcatrain$rotation[19] +testb[i,20]*pcatrain$rotation[20] +testb[i,21]*pcatrain$rotation[21]+testb[i,22]*pcatrain$rotation[22] +testb[i,23]*pcatrain$rotation[23] +testb[i,24]*pcatrain$rotation[24] +testb[i,25]*pcatrain$rotation[25] +testb[i,26]*pcatrain$rotation[26]    
  + testb[i,27]*pcatrain$rotation[27] + testb[i,28]*pcatrain$rotation[28] +testb[i,29]*pcatrain$rotation[29]
  testb1[i,2] = testb[i,1]*pcatrain$rotation[30] + testb[i,2]*pcatrain$rotation[31] +testb[i,3]*pcatrain$rotation[32] +testb[i,4]*pcatrain$rotation[33] +testb[i,5]*pcatrain$rotation[34] +testb[i,6]*pcatrain$rotation[35] +testb[i,7]*pcatrain$rotation[36] +testb[i,8]*pcatrain$rotation[37]+testb[i,9]*pcatrain$rotation[38] +testb[i,10]*pcatrain$rotation[39] +testb[i,11]*pcatrain$rotation[40] +testb[i,12]*pcatrain$rotation[41] +testb[i,13]*pcatrain$rotation[42]   
  + testb[i,14]*pcatrain$rotation[43] + testb[i,15]*pcatrain$rotation[44] +testb[i,16]*pcatrain$rotation[45] +testb[i,17]*pcatrain$rotation[46] +testb[i,18]*pcatrain$rotation[47] +testb[i,19]*pcatrain$rotation[48] +testb[i,20]*pcatrain$rotation[49] +testb[i,21]*pcatrain$rotation[50]+testb[i,22]*pcatrain$rotation[51] +testb[i,23]*pcatrain$rotation[52] +testb[i,24]*pcatrain$rotation[53] +testb[i,25]*pcatrain$rotation[54] +testb[i,26]*pcatrain$rotation[55]    
  + testb[i,27]*pcatrain$rotation[56] + testb[i,28]*pcatrain$rotation[57] +testb[i,29]*pcatrain$rotation[58]
  testb1[i,3] = testb[i,1]*pcatrain$rotation[59] + testb[i,2]*pcatrain$rotation[60] +testb[i,3]*pcatrain$rotation[61] +testb[i,4]*pcatrain$rotation[62] +testb[i,5]*pcatrain$rotation[63] +testb[i,6]*pcatrain$rotation[64] +testb[i,7]*pcatrain$rotation[65] +testb[i,8]*pcatrain$rotation[66]+testb[i,9]*pcatrain$rotation[67] +testb[i,10]*pcatrain$rotation[68] +testb[i,11]*pcatrain$rotation[69] +testb[i,12]*pcatrain$rotation[70] +testb[i,13]*pcatrain$rotation[71]   
  + testb[i,14]*pcatrain$rotation[72] + testb[i,15]*pcatrain$rotation[73] +testb[i,16]*pcatrain$rotation[74] +testb[i,17]*pcatrain$rotation[75] +testb[i,18]*pcatrain$rotation[76] +testb[i,19]*pcatrain$rotation[77] +testb[i,20]*pcatrain$rotation[78] +testb[i,21]*pcatrain$rotation[79]+testb[i,22]*pcatrain$rotation[80] +testb[i,23]*pcatrain$rotation[81] +testb[i,24]*pcatrain$rotation[82] +testb[i,25]*pcatrain$rotation[83] +testb[i,26]*pcatrain$rotation[84]    
  + testb[i,27]*pcatrain$rotation[85] + testb[i,28]*pcatrain$rotation[86] +testb[i,29]*pcatrain$rotation[87]
  testb1[i,4] = testb[i,1]*pcatrain$rotation[88] + testb[i,2]*pcatrain$rotation[89] +testb[i,3]*pcatrain$rotation[90] +testb[i,4]*pcatrain$rotation[91] +testb[i,5]*pcatrain$rotation[92] +testb[i,6]*pcatrain$rotation[93] +testb[i,7]*pcatrain$rotation[94] +testb[i,8]*pcatrain$rotation[95]+testb[i,9]*pcatrain$rotation[96] +testb[i,10]*pcatrain$rotation[97] +testb[i,11]*pcatrain$rotation[98] +testb[i,12]*pcatrain$rotation[99] +testb[i,13]*pcatrain$rotation[100]   
  + testb[i,14]*pcatrain$rotation[101] + testb[i,15]*pcatrain$rotation[102] +testb[i,16]*pcatrain$rotation[103] +testb[i,17]*pcatrain$rotation[104] +testb[i,18]*pcatrain$rotation[105] +testb[i,19]*pcatrain$rotation[106] +testb[i,20]*pcatrain$rotation[107] +testb[i,21]*pcatrain$rotation[108]+testb[i,22]*pcatrain$rotation[109] +testb[i,23]*pcatrain$rotation[110] +testb[i,24]*pcatrain$rotation[111] +testb[i,25]*pcatrain$rotation[112] +testb[i,26]*pcatrain$rotation[113]    
  + testb[i,27]*pcatrain$rotation[114] + testb[i,28]*pcatrain$rotation[115] +testb[i,29]*pcatrain$rotation[116]
  testb1[i,5] = testb[i,1]*pcatrain$rotation[117] + testb[i,2]*pcatrain$rotation[118] +testb[i,3]*pcatrain$rotation[119] +testb[i,4]*pcatrain$rotation[120] +testb[i,5]*pcatrain$rotation[121] +testb[i,6]*pcatrain$rotation[122] +testb[i,7]*pcatrain$rotation[123] +testb[i,8]*pcatrain$rotation[124]+testb[i,9]*pcatrain$rotation[125] +testb[i,10]*pcatrain$rotation[126] +testb[i,11]*pcatrain$rotation[127] +testb[i,12]*pcatrain$rotation[128] +testb[i,13]*pcatrain$rotation[129]   
  + testb[i,14]*pcatrain$rotation[130] + testb[i,15]*pcatrain$rotation[131] +testb[i,16]*pcatrain$rotation[132] +testb[i,17]*pcatrain$rotation[133] +testb[i,18]*pcatrain$rotation[134] +testb[i,19]*pcatrain$rotation[135] +testb[i,20]*pcatrain$rotation[136] +testb[i,21]*pcatrain$rotation[137]+testb[i,22]*pcatrain$rotation[138] +testb[i,23]*pcatrain$rotation[139] +testb[i,24]*pcatrain$rotation[140] +testb[i,25]*pcatrain$rotation[141] +testb[i,26]*pcatrain$rotation[142]    
  + testb[i,27]*pcatrain$rotation[143] + testb[i,28]*pcatrain$rotation[144] +testb[i,29]*pcatrain$rotation[145]
  testb1[i,6] = testb[i,1]*pcatrain$rotation[146] + testb[i,2]*pcatrain$rotation[147] +testb[i,3]*pcatrain$rotation[148] +testb[i,4]*pcatrain$rotation[149] +testb[i,5]*pcatrain$rotation[150] +testb[i,6]*pcatrain$rotation[151] +testb[i,7]*pcatrain$rotation[152] +testb[i,8]*pcatrain$rotation[153]+testb[i,9]*pcatrain$rotation[154] +testb[i,10]*pcatrain$rotation[155] +testb[i,11]*pcatrain$rotation[156] +testb[i,12]*pcatrain$rotation[157] +testb[i,13]*pcatrain$rotation[158]   
  + testb[i,14]*pcatrain$rotation[159] + testb[i,15]*pcatrain$rotation[160] +testb[i,16]*pcatrain$rotation[161] +testb[i,17]*pcatrain$rotation[162] +testb[i,18]*pcatrain$rotation[163] +testb[i,19]*pcatrain$rotation[164] +testb[i,20]*pcatrain$rotation[165] +testb[i,21]*pcatrain$rotation[166]+testb[i,22]*pcatrain$rotation[167] +testb[i,23]*pcatrain$rotation[168] +testb[i,24]*pcatrain$rotation[169] +testb[i,25]*pcatrain$rotation[170] +testb[i,26]*pcatrain$rotation[171]    
  + testb[i,27]*pcatrain$rotation[172] + testb[i,28]*pcatrain$rotation[173] +testb[i,29]*pcatrain$rotation[174]
}

testb1 = testb1[ ,-c(7:29)]
testb1[,7] = SMLtest1[,29]


## testing PCA model with SVM and using betting and money management strategies 

# retest first for best kernel

svm_retest1 = function(kern){
  
  learner = makeLearner("classif.svm", kernel = kern)
  task = makeClassifTask(data = trainb1, target ="V7")
  
  rdesc= makeResampleDesc("CV", iters = 10)
  
  result= resample("classif.svm", task, rdesc, measures = mmce)
  print(result)
  
  
}

svm_retest1("polynomial")

svm_retest1("sigmoid")

svm_retest1("linear")

svm_retest1("radial")


# new grid search for best cost and nu parameters and with kernel from above  

set.seed(47)
learner = makeLearner("classif.svm", kernel = "linear")

ps = makeParamSet(makeIntegerParam("cost", lower = 0.6, upper = 10), makeIntegerParam("nu", lower = -5, upper = 10))

ctrl = makeTuneControlGrid()

task = makeClassifTask(data = trainb1, target ="V7")
rdesc= makeResampleDesc("CV", iters = 3)

res = tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = mmce)


# test model based on results of above 

set.seed(48)
learner = makeLearner("classif.svm", predict.type = "prob", cost = 9, nu = 5, kernel = "linear")

task = makeClassifTask(data = trainb1, target ="V7")

train.set = c(1:nrow(trainb1)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = testb1)

accuracy = performance(prediction, measures = mmce)
print(accuracy)


rdesc= makeResampleDesc("CV", iters = 10)
result= resample("classif.svm", cost = 4, nu = 8, kernel = "polynomial", task, rdesc, measures = mmce)
print(result)



favbsf()

hpbsf()

eprbsf()

mprbsf()

eprkcf()

mprkcf()

eprrkcf()

mprrkcf()

eprbcf()

mprbcf()

favbcf()

hpbcf()


# statistical test of results 

org = c(-2, -2.3, 0.7, 0.3, -1.5, -1.9, -0.3, -1.8, -1.4, -2, 0, -1.8)
pcared = c(-5.8, -5.7, -3.8, -4, -2.9, -2.6, -2.8, -2.9, -3.3, -3.1, -2.8, -3.5)

mean(org)
mean(pcared)

t.test(org, pcared,alternative = "two.sided", var.equal = FALSE)


### comprison of separate player statistics data sets to combined statistics data sets

MLtrainsps = MLtrain1
MLtestsps = MLtest1

ws = c(13,23,89,90,38,39,40,41,43,44,45,46,48,49,50,51,108,109,110,111,129,130,131,132,97,98,125,137,112)
ls = c(17,24,91,92,70,71,72,73,75,76,77,78,80,81,82,83,121,122,123,124,133,134,135,136,99,100,126,138,113)

MLtrainsps[,31] = MLtrainsps[,29]
MLtrainsps[,29] = MLtrainsps[,30]
MLtrainsps[,30] = MLtrainsps[,31]

MLtestsps[,31] = MLtestsps[,29]
MLtestsps[,29] = MLtestsps[,30]
MLtestsps[,30] = MLtestsps[,31]

colnames(MLtrainsps)[29] = "AS_rank"
colnames(MLtrainsps)[30] = "AS_win_lose_binary"

colnames(MLtestsps)[29] = "AS_rank"
colnames(MLtestsps)[30] = "AS_win_lose_binary"

for(i in 1:5325){
  for(j in 1:29){
    if(MLtrainsps[i,30] ==1){
      MLtrainsps[i,j] = tennistr[i, ws[j]]
    } else{
      MLtrainsps[i,j] = tennistr[i, ls[j]]
    }
  }
}


for(i in 1:5325){
  for(j in 1:29){
    if(MLtrainsps[i,30] ==1){
      MLtrainsps[i,j+30] = tennistr[i, ls[j]]
    } else{
      MLtrainsps[i,j+30] = tennistr[i, ws[j]]
    }
  }
}


for(i in 1:2282){
  for(j in 1:29){
    if(MLtestsps[i,30] ==1){
      MLtestsps[i,j] = tenniste[i, ws[j]]
    } else{
      MLtestsps[i,j] = tenniste[i, ls[j]]
    }
  }
}


for(i in 1:2282){
  for(j in 1:29){
    if(MLtestsps[i,30] ==1){
      MLtestsps[i,j+30] = tenniste[i, ls[j]]
    } else{
      MLtestsps[i,j+30] = tenniste[i, ws[j]]
    }
  }
}


## testing model with RF and using betting and money management strategies 

# retest first for optimal hyperparamters

set.seed(49)
learner = makeLearner("classif.randomForest")

ps = makeParamSet(makeIntegerParam("ntree", lower = 10, upper = 200), makeIntegerParam("mtry", lower = 4, upper = 25))

ctrl = makeTuneControlGrid()

task = makeClassifTask(data = MLtrainsps, target ="AS_win_lose_binary")
rdesc= makeResampleDesc("CV", iters = 3)

res = tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control = ctrl, measures = mmce)


# test model based on results of above 

set.seed(50)
learner = makeLearner("classif.randomForest", ntree = 200, mtry = 6, predict.type = "prob")

task = makeClassifTask(data = MLtrainsps, target ="AS_win_lose_binary")

train.set = c(1:nrow(MLtrainsps)) 


model = mlr::train(learner, task, subset = train.set)
prediction = predict(model, newdata = MLtestsps)

accuracy = performance(prediction, measures = mmce)
print(accuracy)

rdesc= makeResampleDesc("CV", iters = 10)
result= resample("classif.randomForest", ntree = 200, mtry = 4, task, rdesc, measures = mmce)
print(result)



favbsf()

hpbsf()

eprbsf()

mprbsf()

eprkcf()

mprkcf()

eprrkcf()

mprrkcf()

eprbcf()

mprbcf()

favbcf()

hpbcf()

# statistical test of results 

cs = c(-7.1, -7.1, -6.3, -6.7, -3.2, -3.4, -6.7, -2.6, -2.2, -2.9, -5.5, -1.2)
sps = c(-8.3, -7.2, -6.1, -6.4, -3.8, -4.4, -7, -3.2, -3.3, -3.8, -7.4, -2.1)

mean(cs)
mean(sps)

t.test(cs, sps,alternative = "two.sided", var.equal = FALSE)



## Please see main Dissertation for explanation and analysis of all results 