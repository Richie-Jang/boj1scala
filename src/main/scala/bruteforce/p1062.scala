package bruteforce

import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer
object p1062 {

  var n = 0
  var k = 0  
  var strs = ArrayBuffer.empty[String]
  var charExists = Array.ofDim[Boolean](26)
  var res = 0
  val charSize = 26

  def stringCheck(): Unit = {
    //println(charExists.mkString(", "))
    def loop(i: Int, limit: Int, s: String): Boolean = {
      if i == limit then true
      else
        val cc = s(i)
        if charExists(cc - 'a') == false then false
        else
          loop(i+1, limit, s)
    }
    var count = 0
    for str <- strs do      
      val sz = str.length
      if loop(0, sz, str) then count += 1
    res = scala.math.max(res, count)
  }

  def dfs(cur: Int, acc: Int, limit: Int): Unit = {
    if acc == limit then
      stringCheck()
    else if cur >= charSize then ()
    else
      if charExists(cur) == false then
        charExists(cur) = true
        dfs(cur+1, acc+1, limit)
        charExists(cur) = false
      end if
      dfs(cur+1, acc, limit)
  }

  def solve(): Unit = {    
    // a,c,i,n,t must
    if k < 5 then 
      println(res)
    else if k == 26 then 
      println(n)
    else
      val remainCount = k - 5
      // setup 5 chars false
      List('a', 'c', 'i', 'n', 't').foreach{ c =>
        charExists(c - 'a') = true
      }
      dfs(1, 0, remainCount)
      println(res)
  }

  def main(args: Array[String]): Unit = {    
    val r = StdIn.readLine.split("\\s+")
    n = r(0).toInt
    k = r(1).toInt    
    for i <- 1 to n do
      val rr = StdIn.readLine.trim
      val sz = rr.length
      strs += rr.substring(4, sz-4)
    end for
    solve()
  }
}