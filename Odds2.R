### 2017/ 2018 odds code

## please change read pathway as required

odds = read.csv("D://Odds3.csv")


for (i in 1:nrow(odds)){
  if (odds[i,7] == "F"){
    odds[i,8] = 1
  }
  if (odds[i,7] == "SF"){
    odds[i,8] = 2
  }
  if (odds[i,7] == "QF"){
    odds[i,8] = 3
  }
  if (odds[i,7] == "R16"){
    odds[i,8] = 4
  }
  if (odds[i,7] == "R32"){
    odds[i,8] = 5
  }
  if (odds[i,7] == "R64"){
    odds[i,8] = 6
  }
  if (odds[i,7] == "R128"){
    odds[i,8] = 7
  }
  if (odds[i,7] == "RR"){
    odds[i,8] = 3
  }
}


path1 = "D:\\Odds4.csv"
write.csv(odds, path1)


# file was tidied and some manipulation of names required and then reread in 

odds2 = read.csv("D://Odds4.csv")




for (j in 1:2637){
  for(i in 1:2637){
    if ((odds2[j,2] == odds2[i,4]) & (odds2[j,6] == odds2[i,7]) & (odds2[j,1] == odds2[i,1])){
      odds2[j,10] = odds2[i,8]
      odds2[j,11] = odds2[i,9]
    }
    if ((odds2[j,3] == odds2[i,5]) & (odds2[j,6] == odds2[i,7]) & (odds2[j,1] == odds2[i,1])){
      odds2[j,10] = odds2[i,8]
      odds2[j,11] = odds2[i,9]
    }
  }    
}

for (j in 1823:1876){
  for(i in 1823:1876){
    if ((odds2[j,2] == odds2[i,4]) & (odds2[j,6] == odds2[i,7]) & (odds2[j,1] == odds2[i,1])){
      odds2[j,10] = odds2[i,8]
      odds2[j,11] = odds2[i,9]
    }
    if ((odds2[j,3] == odds2[i,5]) & (odds2[j,6] == odds2[i,7]) & (odds2[j,1] == odds2[i,1])){
      odds2[j,10] = odds2[i,8]
      odds2[j,11] = odds2[i,9]
    }
  }    
}




path1 = "D:\\Odds4.csv"
write.csv(odds2, path1)

# note code does not line up tour final odds because players can appear twice in same round - so  this was done manually
