package dynamic

import scala.io.StdIn

object p1562 {

  val MAX = 101
  val MAXBIT = 1 << 10
  val DIV = 1_000_000_000
  var n = 0
  val dp = Array.ofDim[Long](MAX, 11, MAXBIT)

  def main(args: Array[String]): Unit = {
    n = StdIn.readLine.trim.toInt
    for i <- 1 until 10 do dp(1)(i)(1 << i) = 1

  }
}
