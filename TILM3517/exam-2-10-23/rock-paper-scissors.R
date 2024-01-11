# Stan and Oliver play a tournament of Rock (R), Paper (P) and Scissors (S) where 
# R beats S, P beats R and S beats P. A player gets one point from a win. No 
# points are given from draws. Both players have their own play patterns which 
# they follow confidently until the game is over:

# Stan always plays RPSRPSP
# Oliver always plays PPSSPSRPRRS

# For example, after the first three rounds Oliver has 1 point, while Stan has 0 
# points. It takes 303 rounds, until Oliver reaches 100 points. How many rounds 
# does it take until either player reaches 5000 points?

stan_seq = c('R', 'P', 'S', 'R', 'P', 'S', 'P')
stan_seq_len = length(stan_seq)
oliver_seq = c('P', 'P', 'S', 'S', 'P', 'S', 'R', 'P', 'R', 'R', 'S')
oliver_seq_len = length(oliver_seq)

defeats = list(R = 'P', S = 'R', P = 'S')

stan_points = 0
oliver_points = 0
round_num = 0

while (stan_points < 5000 && oliver_points < 5000) {
  stan_hand = stan_seq[(round_num) %% stan_seq_len + 1]
  oliver_hand = oliver_seq[(round_num) %% oliver_seq_len + 1]
  
  if (defeats[[stan_hand]] == oliver_hand) {
    oliver_points = oliver_points + 1
  } else if (defeats[[oliver_hand]] == stan_hand) {
    stan_points = stan_points + 1
  }

  round_num = round_num + 1
}

print(round_num)
