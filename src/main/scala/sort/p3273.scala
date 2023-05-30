package sort

import scala.io.StdIn

object p3273:

  var n = 0
  var x = 0
  var arr: Array[Int] = _

  def solve(): Unit = 
    val vs = Array.ofDim[Boolean](n)
    var count = 0

    def search(l: Int, r: Int, v: Int, og: Int): Unit =
      if l > r then ()      
      else
        val m = (l + r) / 2
        if arr(m) == v then
          count += 1
          vs(m) = true
          //println(s"FOUND: ${arr(og)} , ${arr(m)}")
        else
          if arr(m) > v then search(l, m-1, v, og)
          else search(m+1, r,v, og)
    end search

    for i <- 0 until n if ! vs(i) do
      val v = x - arr(i)
      search(i+1, n-1, v, i)
    println(count)
  end solve

  def main(args: Array[String]): Unit =
    n = StdIn.readLine.trim.toInt
    arr = StdIn.readLine.split(' ').map(_.toInt).sorted
    x = StdIn.readLine.trim.toInt    
    solve()
  end main

end p3273
