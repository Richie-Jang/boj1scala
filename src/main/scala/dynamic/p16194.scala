package dynamic

import scala.io.StdIn
import scala.math

object p16194 {

  var n = 0
  var p = Array.emptyIntArray
  var dp = Array.emptyIntArray

  def main(args: Array[String]): Unit = {

    n = StdIn.readLine.trim.toInt
    p = Array.ofDim(n+1)
    dp = Array.ofDim(n+1)

    val ag = StdIn.readLine.split("\\s+").map(_.toInt)
    ag.indices.foreach(i => p(i+1) = ag(i))

    dp(1) = p(1)
    for i <- 2 to n do
      var r = p(i)
      for j <- 1 until i do
        r = math.min(r, dp(i-j) + dp(j))
      end for
      dp(i) = r
    end for

    println(dp(n))

  }

}
