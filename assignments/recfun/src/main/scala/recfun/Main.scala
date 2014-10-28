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
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c > r) {
      Int.MinValue 
    }
    else if (c == 0 || c == r) {
      1
    }
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec def balanceIter(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) {
        count == 0
      }
      else if (count < 0) {
        false
      }
      else {
        balanceIter(chars.tail, chars.head match {
          case '(' => count + 1
          case ')' => count - 1
          case _ => count
        })
      }
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        1
      }
      else if (money <= 0 || coins.isEmpty) {
        0
      }
      else {
        countChangeIter(money, coins.tail) + 
        countChangeIter(money - coins.head, coins)
      }
    }
    countChangeIter(money, coins)
  }
}
