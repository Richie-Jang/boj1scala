package LIS

import scala.io.StdIn

object p2565 {

    def gets: (Int,Int) = {
        val g = StdIn.readLine.trim.split("\\s+").map(_.toInt)
        g(0) -> g(1)
    }

    var arr = Array.empty[(Int,Int)]
    var dp = Array.emptyIntArray

    def main(args: Array[String]) : Unit = {
        val n = StdIn.readLine.trim.toInt
        dp = Array.fill(n)(1)
        arr = Array.tabulate(n) { idx => gets }
        arr = arr.sortBy(_._1)

        for 
            i <- 0 until n
            j <- 0 until i
        do
            val a = arr(i)._2
            val b = arr(j)._2
            if a > b then 
                dp(i) = scala.math.max(dp(i), dp(j)+1)
        end for

        println(n - dp.max)
    }
}