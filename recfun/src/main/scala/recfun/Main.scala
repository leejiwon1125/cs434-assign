package recfun
import common._

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
    if (c==0 || r==c) 1
    else pascal(c,r-1)+pascal(c-1,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_inner(chars:List[Char], lparen_min_rparen:Int):Boolean = {
      if (lparen_min_rparen <0) false
      else {
        if (chars.isEmpty) {
          if (lparen_min_rparen == 0) true
          else false
        }
        else {
          if (chars.head == '(') balance_inner(chars.tail, lparen_min_rparen + 1)
          else if (chars.head == ')') balance_inner(chars.tail, lparen_min_rparen - 1)
          else balance_inner(chars.tail, lparen_min_rparen)
        }
      }
    }

    balance_inner(chars,0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sorted_coins = coins.sortWith(_>_)

    def cC_inner(money: Int, coins: List[Int]):Int={
      if(money==0) 1
      else if (money<0) 0
      else if (coins.isEmpty) 0
      else
      cC_inner(money-coins.head,coins) + cC_inner(money,coins.tail)
    }
    cC_inner(money, sorted_coins)
  }
}
