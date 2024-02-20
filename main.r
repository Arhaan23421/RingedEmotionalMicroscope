data <- read.csv(file = "data.csv")

#Pushups
pushups <- list()
for(x in 1:70)
  {
  pushups[data[x,1]] <- data[x,2]
  }
pushDif <- list()
for(x in 1:70)
{
  if(x %% 2 == 0){
    next
  }
  pushDif[data[x,1]] <- data[x+1,2] - data[x,2]
}
#print("Pushups")
#pushDif

#Pullups
pullups <- list()
for(x in 1:70)
  {
  pullups[data[x,1]] <- data[x,3]
  }
pullupsDif <- list()
for(x in 1:70)
{
  if(x %% 2 == 0){
    next
  }

  pullupsDif[data[x,1]] <- as.numeric(data[x+1,3]) - as.numeric(data[x,3])
}
#print("Pullups")
#pullupsDif



#Squat
Squat <- list()
for(x in 1:70)
  {
  Squat[data[x,1]] <- data[x,6]
  }
SquatDif <- list()
for(x in 1:70)
{
  if(x %% 2 == 0){
    next
  }
  SquatDif[data[x,1]] <- as.numeric(data[x+1,6]) - as.numeric(data[x,6])
}
#print("Squats")
#SquatDif

#To split a string, we use unlist(strsplit(str, del))
#str is the string we are splitting
#del is the delimeter used to split str
#Hang
Hang <- list()
for(x in 1:70)
  {
  Hang[data[x,1]] <- data[x,4]
  }
HangDif <- list()
for(x in 1:70)
{
  if(x %% 2 == 0){
    next
  }
  #Data2 -> ['1', '50']
  Data2 <- unlist(strsplit(data[x,4], ":"))
  Time1 <-(as.numeric((Data2[1]))*60) + as.numeric((Data2[2]))
  Data3 <- unlist(strsplit(data[x+1,4], ":"))
  Time2 <-(as.numeric((Data3[1]))*60) + as.numeric((Data3[2]))
  HangDif[data[x,1]] <- Time2 - Time1
}
#HangDif
#Plank
Plank <- list()
for(x in 1:70)
  {
  Plank[data[x,1]] <- data[x,5]
  }
PlankDif <- list()
for(x in 1:70)
{
  if(x %% 2 == 0){
    next
  }
  #Data2 -> ['1', '50']
  Data2 <- unlist(strsplit(data[x,5], ":"))
  Time1 <-(as.numeric((Data2[1]))*60) + as.numeric((Data2[2]))
  Data3 <- unlist(strsplit(data[x+1,5], ":"))
  Time2 <-(as.numeric((Data3[1]))*60) + as.numeric((Data3[2]))
  PlankDif[data[x,1]] <- Time2 - Time1
}
#PlankDif

#
# get lists of Top5 1 and then subtract them from Top 5 2
#Top5
Top5 <- list()
for(x in 1:70)
  {
  Top5[data[x,1]] <- data[x,8]
  }
#Top5
Top5Dif <- list()
for(x in 1:70)
{
  if(x %% 2 == 0){
    next
  }
  list1 <- unlist(strsplit(data[x,8] , " "))
  list2 <- unlist(strsplit(data[x+1,8] , " "))
  Total1 <- 0
  Total2 <- 0
  for(i in 1:5)
  {
    Total1 <- as.numeric(list1[i]) + Total1
    Total2 <- as.numeric(list2[i]) + Total2
  }
  Top5Dif[data[x,1]] <- Total2 - Total1
}  
Top5Dif

#4 4 4 4 4 -> total2 - total1
#5 4 5 3 3  -> 20