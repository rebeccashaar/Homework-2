#Business Analytics Homework 2
#Rebecca Shaar

#Question 1: Pythagorean Theorem Function
is_pythagorean <- function(a, b, c) {
  if (a*a + b*b == c*c){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#Question 2: Loops
for(n in 1000:10){
  #prime_or_not is a binary variable, where 0 indicates that a number is prime
  prime_or_not = 0
  for (i in 2:floor(sqrt(n))) {
    if (n%%i == 0) {
      prime_or_not = 1
    } 
  }
  if(prime_or_not == 0){
    print(n)
  }
}

#Extra Credit: Function to determine is a number is prime
is_prime <- function(n){
  prime_or_not = 0
  for (i in 2:floor(sqrt(n))) {
    if (n%%i == 0) {
      prime_or_not = 1
    } 
  }
  if(n < 0){
    prime_or_not = 1
  }
  if(n == 2){
    prime_or_not = 0
  }
  if(n == 1){
    prime_or_not = 1
  }
  if(prime_or_not == 0){
    print(paste(n, "is prime"))
  } else {
    print(paste(n, "is not prime"))
  }
}

#Question 3: Professor Xavi's Wine Emporium
#Creating the inventory and price matrices
inventory_data = c(20, 30, 50,
                   30, 20, 60,
                   30, 30, 32)

inventory = matrix(inventory_data, nrow = 3, ncol = 3, byrow = TRUE)

price_data = c(5, 45, 10)

price = matrix(price_data, nrow = 3, ncol = 1, byrow = TRUE)

#3A. Calculating the price for each blend
price_blendA = inventory[1,1]*price[1,1] + 
               inventory[1,2]*price[2,1] +
               inventory[1,3]*price[3,1]

price_blendB = inventory[2,1]*price[1,1] + 
  inventory[2,2]*price[2,1] +
  inventory[2,3]*price[3,1]

price_blendC = inventory[3,1]*price[1,1] + 
  inventory[3,2]*price[2,1] +
  inventory[3,3]*price[3,1]

answer <- paste("The price of Blend A is $", price_blendA, "\n",
                "The price of Blend B is $", price_blendB, "\n",
                "The price of Blend C is $", price_blendC, "\n", sep="")
cat(answer)

#3B. Total price of 10 of Blend A, 4 of Blend B, and 5 of Blend C
total_price = 10 * price_blendA + 
              4  * price_blendB + 
              5  * price_blendC
print(paste("The total price is $", total_price, sep = ""))
