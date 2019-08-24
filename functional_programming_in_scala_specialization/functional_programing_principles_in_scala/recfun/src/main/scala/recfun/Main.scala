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
   */
  def pascal(c: Int, r: Int): Int = {
    if(r == 0 && c == 0) 1
    else if (c < 0 || c > r + 1) 0
    else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def r_balance(chars: List[Char], openParenthesis: Int): Int = {
      if (chars.isEmpty || openParenthesis < 0) openParenthesis
      else if (chars.head == '(') r_balance(chars.tail, openParenthesis + 1)
      else if (chars.head == ')') r_balance(chars.tail, openParenthesis - 1)
      else r_balance(chars.tail, openParenthesis)
    }
    r_balance(chars, 0) == 0
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(money > 0 && !coins.isEmpty) {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    else 0
  }

  // def countChange(money: Int, coins: List[Int]): Int = {
  //   if (coins.isEmpty || money <= 0) 0
  //   else {
  //     coins.map(x => {
  //       if (money - x == 0) 1
  //       else countChange(money - x, coins)
  //     }).sum
  //   }

  }
