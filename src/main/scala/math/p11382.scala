package math

import scala.io.StdIn

object p11382 {

  def main(args: Array[String]): Unit = {
    val arr = StdIn.readLine.trim.split("\\s+").map(a => BigInt(a))
    println(arr.sum)
  }

}
