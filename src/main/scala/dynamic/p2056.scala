package dynamic

import scala.io.StdIn
import scala.math

object p2056 {

  var n = 0
  def getArr(): Array[Int] = StdIn.readLine().split("\\s+").map(_.toInt)

  def main(args: Array[String]): Unit = {

    n = StdIn.readLine().trim.toInt
    val dp = Array.ofDim[Int](n+1)
    var res = 0
    for i <- 1 to n do
      val r = getArr()
      dp(i) = r(0)
      for j <- 0 until r(1) do
        val idx = r(j+2)
        dp(i) = math.max(dp(i), dp(idx) + r(0))
      end for

      res = math.max(res, dp(i))
    end for

    println(res)

  }
}
