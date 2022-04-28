#Each row of window.matrix corresponds with a reel, and the columns correspond  
#to top, middle, and bottom of the display stops. The elements correspond the  
#the row number of the symbol to be displayed on reel.matrix. Using this, we 
#display the output of the slot machine. 
SlotDisplay<-function(reel.matrix, window.matrix){
  slot.disp<-c(reel.matrix[window.matrix[1, 1], 1],
               reel.matrix[window.matrix[1, 2], 1],
               reel.matrix[window.matrix[1, 3], 1],
               reel.matrix[window.matrix[2, 1], 2],
               reel.matrix[window.matrix[2, 2], 2],
               reel.matrix[window.matrix[2, 3], 2],
               reel.matrix[window.matrix[3, 1], 3],
               reel.matrix[window.matrix[3, 2], 3], 
               reel.matrix[window.matrix[3, 3], 3]) 
  slot.matrix<-matrix(slot.disp, ncol=3, nrow=3)
  colnames(slot.matrix)<-c("Reel 1", "Reel 2", "Reel 3")
  return(slot.matrix)
}

WindowSpin<-function(window.matrix){ 
  #Generating 3 random integers from 1 to 18. These will be used to randomly 
  #show different sections of our reel. 
  Sys.sleep(.1)#sleeping system for .1 seconds to ensure that the system time
  #differs so our seeds are not the same
  set.seed(Sys.time())#setting a seed
  spin<-sample(1:14, 3, replace = T)#choosing three numbers from 1-14
  #In window.matrix, each row corresponds to a reel. So using sweep, we add the 
  #first element of spin to the each of the elements in the first row of 
  #window.matrix, the 2nd and 3rd elements are added to the 2nd and 3rd rows 
  #respectively. Any element of the output matrix whose value is greater then the 
  #size of the reel needs its value subtracted by the size of the reel to correspond 
  # with a symbol. 
  window.disp<-sweep(x=window.matrix, 1, spin, FUN = "+")
  window.disp[window.disp>14]<-window.disp[window.disp>14]-14
  return(window.disp)
}
#The function PayOutCalc calculates the payout of the slot machine
PayOutCalc<-function(slot.matrix){
  #The function grep returns the indexes where the elements match the input
  #by taking the length of the grep function we can determine how many of a 
  #specific symbol is on a payline
  one.check<-length(grep(1, slot.matrix[2, ]))
  two.check<-length(grep(2, slot.matrix[2, ]))
  three.check<-length(grep(3, slot.matrix[2, ]))
  four.check<-length(grep(4, slot.matrix[2, ]))
  five.check<-length(grep(5, slot.matrix[2, ]))
  six.check<-length(grep(6, slot.matrix[2, ]))
  seven.check<-length(grep(7, slot.matrix[2, ]))
  if(one.check==2){
    pay.winnings=2# exactly two 1s, payout is 2
    return(pay.winnings)#end function return account balance
  }else if(one.check==3){ #exactly three 1s, payout is 3
    pay.winnings=3 
    return(pay.winnings)#end function return account balance
  }else if(one.check==1){ #exactly one 1s, payout is 1
    pay.winnings=1
    return(pay.winnings)#end function return account balance
  }else if(two.check==3){#exactly three 2s, payout is 4
    pay.winnings=4
    return(pay.winnings)#end function return account balance
  }else if(three.check==3){#exactly three 3s, payout is 5
    pay.winnings=5
    return(pay.winnings)#end function return account balance
  }else if(four.check==3){#exactly three 4s, payout is 6
    pay.winnings=6
    return(pay.winnings)#end function return account balance
  }else if(five.check==3){#exactly three 5s, payout is  10
    pay.winnings=10
    return(pay.winnings)#end function return account balance
  }else if(six.check==3){#exactly three 6s, payout is 25
    pay.winnings=25
    return(pay.winnings)#end function return account balance
  }else if(seven.check==3){#exactly three 7s, payout is 100
    pay.winnings=100
    return(pay.winnings)#end function return account balance
  }else {# no hits on the payline
    return(0)#end function return account balance
  }
}

#the function PromptFunc takes in no variables. It prints a list of options
#for the user to choose from and takes a integer as the input. The function 
#outputs the users choice
PromptFunc<-function(){
  writeLines(#displaying prompt
    "Option 1: Output your credit balance
Option 2: Deposit more Credits
Option 3: Spin slot machine for 1 Credit
Option 4: Cash out")
  
  option<-as.integer(readline(prompt = "Enter option number: "))#asking 
  #user for input
  return(option)
}

#this function takes in the variables option which is a integer representing
#what the user wants to program to do.Output which is a list of length 3, the
#first variable being end_bool which is a integer, this variable is used to 
#break the while loop to stop the slot machine.The second variable is balance 
#which represents how many credits the user has. The third variable is 
#window.matrix whose rows represent the idexes corresponding to which section
#of that reel is being displayed
PromptOption<-function(option, output, reel.matrix){
  if(option==1){#if your option is to Output your credit balance
    cat("Your current credit balance is: ", output[[2]])
    cat("\n")
    return(output)
  }else if(option==2){#if your option is to deposit more credits
    deposit<-as.integer(#asking user for how many credits they want to deposit
      readline(prompt="Enter the amount(integers) of credits to deposit: "))
    output[[2]]<-output[[2]]+deposit
    cat("Your current credit balance is: ", output[[2]])
    cat("\n")
    return(output)
  }else if(option==3){#if your option is to spin the slot
    if(output[[2]]>=1){#this if statement is for if you have atleast 1 credit
      output[[3]]<-WindowSpin(output[[3]])#WindowSpin randomly spins the reels
      slot.matrix<-SlotDisplay(reel.matrix,output[[3]])#slot_display returns 
      #a matrix of the window display of the slot machine
      print("The slot window display is: ")
      print(slot.matrix)
      pay<-PayOutCalc(slot.matrix)#calculating how much you won
      output[[2]]<-output[[2]]+pay-1#account balance = accounnt balance+pay-1
      cat("You won: ",pay)
      cat("\n")
      cat("Your current credit balance is: ",output[[2]])
      cat("\n")
      return(output)
    }else{#if you dont have sufficient credits
      print("Insufficient credits, deposit more")
      return(output)
    }
  }else if(option==4){#cashing out of the slot
    cat("Your cashing out with: ", output[[2]])
    cat("\n")
    output[[1]]<-1
    return(output)
  }else{#improper option entered
    print("Improper option entered")
    cat("\n")
    return(output)
  }
}

#the function SlotManager takes in the variables reel.matrix which is a matrix 
#where the columns hold the stops of each reel. window.matrix is the matrix 
#where Each row of window.matrix corresponds with a reel, and the columns 
#correspond  to top, middle, and bottom of the display stops.
SlotManager<-function(reel.matrix, window.matrix){
  account_balance<-0 # setting account balance to 0
  slot.matrix<-SlotDisplay(reel.matrix, window.matrix)#creating the window
  #display of the slot machine
  print(slot.matrix)#printing window display of the slot machine
  
  output<-list(0, account_balance, window.matrix)#creating a list where the first 
  #element is used as a variable to stop the slot machine. The second variable
  #is the users account balance. The third variable is window matrix
  names(output)<-c("End_bool", "balance", "window.matrix")
  
  
  while(output[[1]]==0){#this while loop will keep going until output[[1]], which
    #is the variable used to stop this loop, is equal to zero
    option<-PromptFunc()#asking the user for which option he wants
    output<-PromptOption(option, output, reel.matrix)#executing which option
    #the user wants
  }
}