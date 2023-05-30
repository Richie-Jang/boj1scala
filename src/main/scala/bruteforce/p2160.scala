package bruteforce

import scala.io.StdIn

object p2160 {

  var n = 0
  var pics = Array.empty[Array[Array[Char]]]

  def printPic(idx: Int): Unit = {
    val p = pics(idx)
    println(p.map { _.mkString("") }.mkString("\n"))
  }

  def makeComb(): List[(Int,Int)] = {
    var res = List.empty[(Int,Int)]
    for i <- 0 until n do
      for j <- i+1 until n do
        res = (i, j) :: res
    res
  }

  def checkDifferenceCount(i1: Int, i2: Int): Int = {
    var count = 0
    val p1 = pics(i1)
    val p2 = pics(i2)
    for y <- 0 until 5 do
      for x <- 0 until 7 do
        if p1(y)(x) != p2(y)(x) then count += 1
    count
  }

  def main(args: Array[String]): Unit = {
    n = StdIn.readLine().trim.toInt
    pics = Array.ofDim(n)

    var count = 0
    var idx = -1
    var idxy = -1
    for i <- 1 to n*5 do
      if count == 0 then
        idx += 1
        pics(idx) = Array.ofDim(5)
        idxy = -1
      end if
      val r = StdIn.readLine().trim.toCharArray
      idxy += 1
      pics(idx)(idxy) = r
      count += 1
      if count % 5 == 0 then count = 0
    end for

    val res =
      makeComb().map { k =>
        val (a,b) = k
        (a,b,checkDifferenceCount(a,b))
      }.sortBy(k => k._3).head
    println(s"${res._1+1} ${res._2+1}")
  }

}
