package math

import scala.io.StdIn

object p17496 {

    def getDays(cur: Int, step: Int, limit: Int, count: Int): Int = {
        if cur > limit then count-1
        else
            getDays(cur+step, step, limit, count+1)
    }

    def main(args: Array[String]): Unit = {
        val arr = StdIn.readLine.trim.split(' ').map(_.toInt)
        val days = getDays(1, arr(1), arr(0), 0)
        println(days * arr(2) * arr(3))
    }
}