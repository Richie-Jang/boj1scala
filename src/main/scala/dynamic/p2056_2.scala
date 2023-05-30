package dynamic

import java.util
import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.math

object p2056_2 {

  var n = 0
  var indegree = Array.emptyIntArray
  var times = Array.emptyIntArray
  var adjs = Array.empty[ListBuffer[Int]]

  def solve(): Unit = {
    val q = new util.LinkedList[Int]()
    val resArr = Array.ofDim[Int](n+1)
    for i <- 1 to n do
      resArr(i) = times(i)
      if indegree(i) == 0 then q.offer(i)
    end for

    while !q.isEmpty do
      val cur = q.poll()
      adjs(cur).foreach { nt =>
        indegree(nt) -= 1
        resArr(nt) = math.max(resArr(nt), resArr(cur) + times(nt))
        if indegree(nt) == 0 then q.offer(nt)
      }
    end while

    val res = (1 to n).foldLeft(0) { (acc, i) => math.max(acc, resArr(i)) }

    println(res)
  }

  def main(args: Array[String]): Unit = {
    n = StdIn.readLine().trim.toInt

    indegree = Array.ofDim(n+1)
    times = Array.ofDim(n+1)
    adjs = Array.tabulate(n+1){ _ => ListBuffer.empty[Int] }

    for i <- 1 to n do
      val r = StdIn.readLine().split("\\s+").map(_.toInt)
      times(i) = r(0)
      for j <- 0 until r(1) do
        val idx = r(j+2)
        adjs(idx).addOne(i)
        indegree(i) += 1
      end for
    end for

    solve()

  }

}
