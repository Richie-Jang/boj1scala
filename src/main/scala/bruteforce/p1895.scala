package bruteforce

import scala.io.StdIn
import scala.util.ChainingOps

object p1895 {

  var r,c,t = 0
  var pic = Array.empty[Array[Int]]

  def gets() = StdIn.readLine().split("\\s+").map(_.toInt)

  def searchMid(x: Int, y: Int): Int = {
    var list = List.empty[Int]
    for i <- y until y+3 do
      for j <- x until x+3 do
        list = pic(i)(j) :: list
    val l = list.sorted
    l(4)
  }
    
  def main(args: Array[String]): Unit = {    
    
    ChainingOps(gets()).tap{ a => 
      r = a(0)
      c = a(1)
    }

    pic = Array.ofDim(r)
    for i <- 0 until r do
      pic(i) = gets()
    t = gets()(0)
    var count = 0
    for y <- 0 to r-3 do
      for x <- 0 to c-3 do
        val search = searchMid(x, y)
        if search >= t then count += 1
    println(count)
  }
}
