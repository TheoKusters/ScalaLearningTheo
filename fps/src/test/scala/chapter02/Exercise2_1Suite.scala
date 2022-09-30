package chapter02

import chapter02.MyCalcFunctions.*
import chapter02.FormatCalcFunctions.*

class Exercise2_1Suite {

  assert(formatResult("absolute value", -42, abs) == "The absolute value of -42 is 42", 
    "Error in the abs function")
  assert(formatResult("factorial", 7, factorial) == "The factorial of 7 is 5040",
    "Error in the factorial function)
  assert(formatResult("Fibonacci value in position", 8, fib) == "The 8th Fibonacci number is 13",
    "Error in the fib function")
}
