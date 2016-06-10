package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    * Check for the value of a number given its position in pascal's triangle
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    * Check whether the given string has valid parentheses
    */
  def balanceStepper(runningTotal: Int, chars: List[Char]): Int = {
    if (chars.isEmpty) runningTotal
    else if (runningTotal < 0) -1
    else if (chars.head == '(') balanceStepper(runningTotal + 1, chars.tail)
    else if (chars.head == ')') balanceStepper(runningTotal - 1, chars.tail)
    else balanceStepper(runningTotal, chars.tail)
  }

  def balance(chars: List[Char]): Boolean = {
    return balanceStepper(0, chars) == 0
  }

  /**
    * Exercise 3
    * Given an amount and a list of coin denominations, determine how many ways there are to compose that amount
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
