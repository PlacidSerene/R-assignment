library(dplyr)
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


game1 <- function(time=1:1000){
  df <- data.frame()
  for (i in time){
    df <- rbind(df, play(i))
  }
  df <- df %>% group_by(time) %>% summarise(player1=paste(player1, collapse = ''), player2=paste(player2, collapse = ''))
  return(df)
}

# I hate to do it this way, but it left me no choice -.-
# Generally speaking, this list is overdo because in some scenarios, p1 cannot have these numbers.
# For example: p1 will never have number 789, because after the first 7, p2 will take 8, and so on.
# I just don't want to think about it too much. This is complicated enough

win_check <- function(player1,player2) {
  win_list <- c(123,132,213,231,312,321,
                456,465,546,564,645,654,
                789,798,879,897,978,987,
                147,174,417,471,741,714,
                258,285,528,582,825,852,
                369,396,639,693,936,963,
                159,195,519,591,915,951,
                357,375,537,573,735,753)
  if(player1 %in% win_list){
    return('P1')
  }else if(player2 %in% win_list){
  return('P2')
  }else{
    return('Draw')
  }
}

# Setup the game 
df <- game1()

# Convert to numeric value
df$player1 <- as.numeric(df$player1)
df$player2 <- as.numeric(df$player2)

# Create winner column, this step is really painful btw 
df$winner <- mapply(FUN = win_check, player1=df$player1, player2=df$player2)

# Let's see the result
result <- df %>% group_by(winner) %>% summarise(count=n())





