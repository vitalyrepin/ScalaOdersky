package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("()() example".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if ((c == 0) || (c == r)) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countBrackets(xs: List[Char], s: Int): Int = {
      if ((s < 0) || (xs.isEmpty)) s else {
        if (xs.head == '(') countBrackets(xs.tail, s + 1) else {
          if (xs.head == ')') countBrackets(xs.tail, s - 1) else countBrackets(xs.tail, s)
        }
      }
    }
    countBrackets(chars, 0) == 0
  }
  
  @tailrec def g(s: Int, n: Int): Int = if(n == 1) s else g(s*n, n-1)                                              
  def f(n: Int): Int = g(1, n)      
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countWays(m: Int, coins: List[Int], s: Int): Int = {
      if (m == 0) 1 else {
        if (m < 0) 0 else {
          if (coins.isEmpty) 0 else s + countWays(m, coins.tail, s) + countWays(m - coins.head, coins, s)
        }
      }
    }
    countWays(money, coins, 0)
  }
}
