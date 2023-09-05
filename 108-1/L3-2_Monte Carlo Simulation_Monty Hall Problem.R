##Monte Carlo Simulation
#https://www.mathwarehouse.com/monty-hall-simulation-online/
rm(list = ls())
#Monty Hall (Original)
#把問題寫成換與不換為input，有沒有得到car為output的fcn
monty_hall = function(switch = logical()){
  #randomly arrange the door
  doors = c(1,2,3)
  names(doors) = sample(c("goat", "car", "goat"))
  prize_door = which(names(doors)=='car') #find the index of car
  #Now Guess
  guess = sample(doors, 1)
  if(guess == prize_door){
    revealed_door = sample(doors[doors != prize_door], 1) #randomly open one of the goat door
  }else{
    revealed_door = doors[doors != prize_door & doors != guess] #open the other goat door
  }
  #Show the return
  if(switch){
    switched_door = doors[doors != guess & doors != revealed_door]
    return(prize_door == switched_door)
  } else {
    return(prize_door == guess)
  }
}

mean(replicate(monty_hall(T), n = 10000))
mean(replicate(monty_hall(F), n = 10000))
result = replicate(monty_hall(T), n = 10000)
barplot(c(sum(result), length(result)-sum(result)), 
        main = 'The frequency of winning the car in 
        Monty Hall Problem in 10000 rounds',names.arg = c('Change', 'Do not change'), ylim = c(0,10000))

#(Optional)
#Extend to a n door problem, choose 1 door and open n-2 doors
n = 4
monty_hall_n = function(switch = logical(), n){
  #randomly arrange the door
  doors = 1:n
  names(doors) = sample(rep(c("goat", "car"), c(n-1,1)))
  prize_door = which(names(doors)=='car') #find the index of car
  #Now Guess
  guess = sample(doors, 1)
  if(guess == prize_door){
    revealed_door = sample(doors[doors != prize_door], n-2) #randomly open one of the goat door
  }else{
    #revealed_door = sample(doors[doors != prize_door & doors != guess], n-2)
    revealed_door = doors[!doors %in% c(prize_door, guess)] #open the other goat door
  }
  #Show the return
  if(switch){
    switched_door = doors[!doors %in% c(guess, revealed_door)]#'%in%' is value matching
    return(prize_door == switched_door)
  } else {
    return(prize_door == guess)
  }
}
monty_hall_n(T, 3)
mean(replicate(monty_hall_n(T, 4), n = 10000))
mean(replicate(monty_hall_n(F, 4), n = 10000))
mean(replicate(monty_hall_n(T, 100), n = 10000))
mean(replicate(monty_hall_n(F, 100), n = 10000))
result = replicate(monty_hall_n(T, 100), n = 10000)
barplot(c(sum(result), length(result)-sum(result)),
        main = 'The frequency of winning the car in 
        Monty Hall Problem with 100 doors in 10000 rounds',
        names.arg = c('Succeed', 'Fail'), ylim = c(0,10000))


#Further Discussion: What if the decision of 'change' is decided randomly?
monty_hall(sample(c(T, F), 1))
mean(replicate(expr = monty_hall(sample(c(T, F), 1)), 10000))
#the prob of win becomes 0.5

0.9*(2/3)+0.1*(1/3)
mean(replicate(expr = monty_hall(sample(c(T, F), 1, prob = c(0.9, 0.1))), 10000))
0.1*(2/3)+0.9*(1/3)
mean(replicate(expr = monty_hall(sample(c(T, F), 1, prob = c(0.1, 0.9))), 10000))
0.4*(2/3)+0.6*(1/3)
mean(replicate(expr = monty_hall(sample(c(T, F), 1, prob = c(0.4, 0.6))), 10000))

#(Optional)
#For n doors
monty_hall_n(sample(c(T, F), 1), n = 100)
mean(replicate(expr = monty_hall_n(sample(c(T, F), 1), n = 100), 10000))
#the prob of win becomes 0.5
#the intuitive thought behind this result is that your decision do not depends on new information
#because you do not use new info inflow at all => you won't gain from info inflow

mean(replicate(expr = monty_hall_n(sample(c(T, F), 1, prob=c(0.9,0.1)), n = 100), 10000))
