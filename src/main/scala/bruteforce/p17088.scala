package bruteforce

import scala.annotation.tailrec
import scala.io.StdIn
import scala.math

object p17088 {

  var n = 0
  var arr = Array.empty[Int]

  def solve(): Unit = {

    def checkArr(narr: Array[Int]): Int = {
      var count = 0
      val diff = narr(1) - narr(0)
      for i <- 1 until narr.length-1 do
        val d = narr(i+1) - narr(i)
        if diff != d then
          val dd = math.abs(diff - d)
          if dd > 1 then return -1
          narr(i+1) += (d - diff)
          count += 1
        end if
      end for
      count
    }

    var min = Int.MaxValue

    val diffArr = arr.sliding(2).toArray.map { a => a(1) - a(0) }
    val checked = diffArr.sliding(2).exists{a => math.abs(a(1)-a(0)) > 4}
    if checked then println("-1")
    else
      for i <- List(-1,0,1) do
        for j <- List(-1,0,1) do
          var count = 0
          val narr = arr.clone()
          narr(0) += i
          narr(1) += j
          if i != 0 then count += 1
          if j != 0 then count += 1
          var ans = checkArr(narr)
          if ans != -1 then
            ans += count
            if min > ans then min = ans
          end if
        end for
      end for
      println(min)
  }

  def main(args: Array[String]): Unit = {
    n = StdIn.readLine.trim.toInt
    arr = StdIn.readLine.split("\\s+").map(_.toInt)

    if n == 1 || n == 2 then println(0)
    else
      solve()
  }
}
