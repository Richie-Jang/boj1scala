package sort

import scala.io.StdIn

object p11557 {

  def solve(sb: StringBuilder): Unit = {
    val n = StdIn.readLine.trim.toInt
    var maxName = ""
    var maxNum = -1
    for i <- 1 to n do
      val g = StdIn.readLine.split(' ')
      val c = g(1).toInt
      if maxNum < c then
        maxName = g(0)
        maxNum = c
      end if
    end for
    sb.append(maxName).append(System.lineSeparator())
  }

  def main(args: Array[String]): Unit = {
    val t = StdIn.readLine.trim.toInt
    val sb = StringBuilder()
    for i <- 0 until t do
      solve(sb)
    end for
    print(sb.toString)
  }
}
