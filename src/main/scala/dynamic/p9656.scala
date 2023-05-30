package dynamic

import scala.io.StdIn

object p9656 {

  var n = 0
  var dp = Array.emptyBooleanArray

  def solve(): Unit = {
    dp(1) = false
    dp(2) = true
    dp(3) = false
    dp(4) = true
    if n % 2 == 0 then println("SK") else println("CY")
  }

  def main(args: Array[String]): Unit = {
    n = StdIn.readLine.trim.toInt
    dp = Array.ofDim(n+1)
    solve()
  }

}
