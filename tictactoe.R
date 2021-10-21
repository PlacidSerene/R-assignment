set.seed(10)
p2_move <- function(pools, p1_move) {
  p2_moving <- p1_move + 1
  while (!(p2_moving %in% pools)) {
    if (p2_moving <= 9){
      p2_moving <- p2_moving + 1
    }else{
      p2_moving <- 1
    }
  }
  return(p2_moving)}

#start game 
pools <- c(1,2,3,4,5,6,7,8,9)

#first move
p1_first_move <- sample(pools,1)
pools <- pools[pools != p1_first_move]
p2_first_move <- p2_move(pools,p1_first_move)
pools <- pools[pools != p2_first_move]

#second_move
p1_second_move <- sample(pools,1)
pools <- pools[pools != p1_second_move]
p2_second_move <- p2_move(pools,p1_second_move)
pools <- pools[pools != p2_second_move]

#third_move
p1_third_move <- sample(pools,1)
pools <- pools[pools != p1_third_move]
p2_third_move <- p2_move(pools, p1_third_move)
pools <- pools[pools != p2_third_move]


# Create move list 
time <- 1:10
p1 <- c(p1_first_move, p1_second_move, p1_third_move)
p2 <- c(p2_first_move, p2_second_move, p2_third_move)

# game function:
play <- function(time){
  pools <- c(1,2,3,4,5,6,7,8,9)
  #first move
  p1_first_move <- sample(pools,1)
  pools <- pools[pools != p1_first_move]
  p2_first_move <- p2_move(pools,p1_first_move)
  pools <- pools[pools != p2_first_move]
  
  #second_move
  p1_second_move <- sample(pools,1)
  pools <- pools[pools != p1_second_move]
  p2_second_move <- p2_move(pools,p1_second_move)
  pools <- pools[pools != p2_second_move]
  
  #third_move
  p1_third_move <- sample(pools,1)
  pools <- pools[pools != p1_third_move]
  p2_third_move <- p2_move(pools, p1_third_move)
  pools <- pools[pools != p2_third_move]
  
  #p1 and p2 moves:
  p1 <- c(p1_first_move, p1_second_move, p1_third_move)
  p2 <- c(p2_first_move, p2_second_move, p2_third_move)
  row <- data.frame("time"=time,"player1"=p1, "player2"=p2)
  return(row)
}


game1 <- function(time=1:10){
  df <- data.frame()
  for (i in time){
    df <- rbind(df, play(i))
  }
  df <- df %>% group_by(time) %>% summarise(player1=paste(player1, collapse = '-'), player2=paste(player2, collapse = '-'))
  return(df)
}


