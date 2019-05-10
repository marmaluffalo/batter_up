#input averages
batters <- c(200, 250, 300, 350, 400, 500)
#5 guys get 3200 attempts, the PED guy gets half that
games_played <- c(rep(3200, 5), 1600)

careers <- data.frame(batter = batters, games_played = games_played)
careers

game <- function(x){
  hit = 0 
  for (i in 1:4){
    pitch <- sample(1:1000, 1)
    pitch
    if (pitch <= x){
      hit = hit + 1
    }
  }
  if (hit > 0){
    return(1)
  }
  else{
    return(0)
  }
}


for (a in 1:dim(careers)[1]){
  beats = 0
  for (lifetimes in 1:1000000){
    games <- careers$games_played[a]
    average <- careers$batter[a]
    current_streak = 0
    best_streak = 0

    for (k in 1:games){
      if (game(average) == 1){
        current_streak = current_streak + 1
      } else{
          if (current_streak > best_streak){
            best_streak = current_streak
            }
          current_streak = 0
          }
    }
    if (best_streak > 56){
      beats = beats + 1
    }
  }
  print(paste("Mr.", average, "beat Joe", beats, "times"))
}