
do_lstm_step <- function(new_input, old_output, old_state) {

  forget_gate <- plogis(3 + .1 * old_output + 2 * new_input)
  input_gate <- plogis(.5 + -.1 * old_output + .2 * new_input)
  output_gate <- plogis(-.2 + .3 * old_output + .1 * new_input)
  candidate_state <- tanh(0 + .2 * old_output + .1 * new_input) 
  new_state <- forget_gate * old_state + input_gate * candidate_state
  new_output <- output_gate * tanh(new_state)
  return(c(new_state, new_output))
}

state <- 0
y <- numeric(100)
x <- .1 * sin(1:100 / 12)

step <- do_lstm_step(0, 0, 0)

y[1] <- step[2]
state <- step[1]

for (t in 2:100) {
  step <- do_lstm_step(x[t], y[t - 1], state)
  print(state)
  state <- step[1]
  y[t] <- step[2]
}

plot(y)
