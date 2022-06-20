#--------------------INITIAL SETUP---------------------------------------------------------------

lgrid <- matrix(NA, nrow = 8, ncol = 8)
lgrid[1,] <- c("r", "l", "q", "s", "t", "z", "c", "a")
lgrid[2,] <- c("i", "v", "d", "z", "h", "l", "t", "p")
lgrid[3,] <- c("u", "r", "o", "y", "w", "c", "a", "c")
lgrid[4,] <- c("x", "r", "f", "n", "d", "p", "g", "v")
lgrid[5,] <- c("h", "j", "f", "f", "k", "h", "g", "m")
lgrid[6,] <- c("k", "y", "e", "x", "x", "g", "k", "i")
lgrid[7,] <- c("l", "q", "e", "q", "f", "u", "e", "b")
lgrid[8,] <- c("l", "s", "d", "h", "i", "k", "y", "n")


edge_squares = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 24, 25, 32, 33, 40, 41, 48, 49, 56, 57, 58, 59, 60, 61, 62, 63, 64)
green_squares = c(14, 23, 42, 51)

get_start_position <- function() {
  choices <- setdiff(1:64, edge_squares)
  position <- sample(choices, 1 , replace = TRUE)
  return(position)
}

start_position <- get_start_position()

# ------------------------PART-1----------------------------------------------------------------

move_if_on_edge <- function() {
  position <- sample(1:64, 1, replace = TRUE)
  return(position)
}
move_if_not_on_edge <- function(square) {
  dist_from_adj_squares <- c(1, 7, 8, 9)
  adj_squares_1 <- square - dist_from_adj_squares
  adj_squares_2 <- square + dist_from_adj_squares
  adj_squares <- c(adj_squares_1, adj_squares_2)
  position <- sample(adj_squares, 1, replace = TRUE)
  return(position)
}

play_next_turn <- function(position) {
  if (position %in% edge_squares) next_position <- move_if_on_edge() else 
    next_position <- move_if_not_on_edge(position)
  return(next_position)
} 

next_square <- play_next_turn(16)

#-------------------------PART-3--------------------------------------------------------------

letter_collection <- c()
available_letters <- c()
num_letters <- 0
count <- 0

handle_green_squares <- function(square, p){
  prob <- sample(c(p, 1-p), 1, replace = TRUE)
  
  if(prob == p){
    letter_collection <<- c("f", "f")
    count <<- 1
    available_letters <<- c("h", "k")
    num_letters <<- 3
  }
  
  else if(prob == 1-p){
    if(lgrid[square] %in% available_letters){
      available_letters <<- available_letters[available_letters != lgrid[square]]
      num_letters <<- num_letters -1
    }
    
    if(lgrid[square] %in% letter_collection){
      letter_collection <<- letter_collection[letter_collection != lgrid[square]]
      count <<- max(0, length(letter_collection) - 1)
    }
  }
}

handle_white_squares <- function(square){
  if(lgrid[square] %in% available_letters && count < 2){
    letter_collection <<- append(letter_collection, c(lgrid[square], lgrid[square]))
    available_letters <<- available_letters[available_letters != lgrid[square]]
    count <<- count + 1 
  }
  
  else if(num_letters < 3){
    available_letters <<- append(available_letters, lgrid[square])
    num_letters <<- num_letters + 1
  }
}

pick_or_leave_letter <- function(square, p){
  if(square %in% green_squares) handle_green_squares(square, p) else
    handle_white_squares(square)
}

count_num_moves <- function(start, p){
  letter_collection <<- c()
  available_letters <<- c()
  num_letters <<- 0
  count <<- 0
  curr_position <- start
  end_game <- FALSE
  moves <- 0
  
  while(end_game == FALSE){
    if(count == 2 && length(available_letters) == 1){
      letter_collection <<- append(letter_collection, available_letters)
      available_letters <<- c()
      end_game <- TRUE
    }
    
    else{
      pick_or_leave_letter(curr_position, p)
      next_pos <- play_next_turn(curr_position)
      curr_position <- next_pos
      moves <- moves + 1
    }
  }
  
  return(moves)
}


#-----------------------------------PART-4------------------------------------------------
# Setting starting position as D4 i.e 28

df <- data.frame(p = c(), moves = c())
for (i in 1:20) {
  p <- round(runif(1), 3)
  moves <- count_num_moves(28,p)
  df <- rbind(df, data.frame(p = p, moves = moves))
}
ggplot(aes(p,moves), data = df) + geom_line() + geom_point() + labs(title = "Fig. 5.2.a Probability v/s Number of moves")

#---------------------------------PART-5--------------------------------------------------
sample_results <- replicate(100, count_num_moves(46,0.05))
sample_results2 <- replicate(100, count_num_moves(28, 0.95))
results1 <- data.frame(p = 0.05, moves = sample_results)
results2 <- data.frame(p = 0.95, moves = sample_results2)
summary(sample_results)
summary(sample_results2)
ggplot(aes(moves, p, color = 'results1'), data = results1) + geom_point() +
  geom_point(aes(moves,p, color = 'results2'), data = results2) 

#---------------------------------PART-6--------------------------------------------------
Xa <- c(25, 13, 16, 24, 11, 12, 24, 26, 15, 19, 34)
Xb <- c(35, 41, 23, 26, 18, 15, 33, 42, 18, 47, 21, 26)
p <- 0.5

par(mfrow=c(1,2))
hist(sample_results)
hist(sample_results2)
plot1 + plot2
grid.arrange(plot1, plot2, ncol=2)