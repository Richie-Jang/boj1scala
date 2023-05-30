package dynamic

import scala.io.StdIn
import scala.math

object p9465 {

  val dp = Array.ofDim[Int](2, 100_000)
  var n = 0
  var t = 0

  def solve(): Unit = {
    n = StdIn.readLine().trim.toInt

    for y <- 0 until 2 do
      for x <- dp(0).indices do
        dp(y)(x) = 0

    val a1 = StdIn.readLine.split("\\s+").map(_.toInt)
    val a2 = StdIn.readLine.split("\\s+").map(_.toInt)

    dp(0)(0) = a1(0)
    dp(1)(0) = a2(0)

    dp(0)(1) = dp(1)(0) + a1(1)
    dp(1)(1) = dp(0)(0) + a2(1)

    for i <- 2 until n do
      dp(0)(i) = math.max(dp(1)(i-1), dp(1)(i-2)) + a1(i)
      dp(1)(i) = math.max(dp(0)(i-1), dp(0)(i-2)) + a2(i)

    println(math.max(dp(0)(n-1), dp(1)(n-1)))
  }

  def main(args: Array[String]): Unit = {
    t = StdIn.readLine().trim.toInt
    for i <- 0 until t do solve()
  }
}
