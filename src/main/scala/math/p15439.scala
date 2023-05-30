package math

import scala.io.StdIn

object p15439 {
    def main(args: Array[String]): Unit = {
        val n = StdIn.readLine.trim.toInt
        val res = n * (n-1)
        println(res)
    }
}

