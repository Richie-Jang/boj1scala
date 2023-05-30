package math

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.collection.mutable.HashSet

object p1033 {
  var n = 0
  val MAX = 10
  val res = Array.fill(MAX)(1L)
  var graph = Array.tabulate(MAX)(_ => ArrayBuffer.empty[Int])
  val visits = HashSet.empty[(Int,Int)]
  val inps = Array.tabulate(MAX)(_ => (0,0,0,0))

  def getArr: Array[Int] =
    StdIn.readLine.split("\\s+").map(_.toInt)

  def dfs(cur: Int, v: Int, pos: Int) : Unit = 
    val k = cur -> v
    if visits.contains(k) then ()
    else
      visits.add(k)
      res(cur) = res(cur) * v
      for next <- graph(cur) do
        if next != pos then
          dfs(next, v, cur)
      
  def gcd(v1: Long, v2: Long): Long =
    if v2 == 0 then v1
    else
      gcd(v2, v1 % v2)

  def main(args: Array[String]): Unit = 

    n = StdIn.readLine.trim.toInt
    for i <- 0 until n-1 do
      val Array(a,b,p,q) = getArr
      graph(a) += b
      graph(b) += a
      inps(i) = (a,b,p,q)
    
    for i <- 0 until n-1 do
      val (a,b,p,q) = inps(i)
      visits.clear()
      dfs(a, p, b)
      dfs(b, q, a)

    var g = res(0)
    for i <- 0 until n do
      g = gcd(g, res(i))
    for i <- 0 until n do
      res(i) = res(i) / g

    println(res.take(n).mkString(" "))
  end main
}
