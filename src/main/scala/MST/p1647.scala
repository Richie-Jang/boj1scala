package MST

import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue

import java.io.BufferedReader
import java.io.InputStreamReader

object p1647:

  var n = 0
  var m = 0  
  var br = new BufferedReader(new InputStreamReader(System.in))

  val gets = { () =>
    val g = br.readLine.split(' ')
    (g(0).toInt, g(1).toInt, g(2).toInt)
  }

  // (node, weight)
  val adjs = ArrayBuffer.empty[ArrayBuffer[(Int, Int)]]

  def primAlgo(): Unit =
    given order: Ordering[(Int,Int)] with
      def compare(a: (Int,Int), b: (Int,Int)): Int =
        if a._2 == b._2 then -1 * a._1.compareTo(b._1) else -1 * (a._2.compareTo(b._2))

    val visits = Array.ofDim[Boolean](n+1)
    val pq = PriorityQueue[(Int,Int)]()(using order)

    adjs(1).foreach(a => pq.enqueue(a))
    visits(1) = true
    
    var mst = 0
    var largestWeight = 0
    var connectCount = 0

    def loop(): Unit =
      if connectCount == n-1 || pq.isEmpty then ()
      else
        val (nn, w) = pq.dequeue
        //println(s"Visited : ${nn}")
        if !visits(nn) then
          visits(nn) = true
          largestWeight = math.max(largestWeight, w)
          mst += w
          connectCount += 1
          adjs(nn).foreach(a => if !visits(a._1) then pq.enqueue(a))    
        end if
        loop()

    loop()
    println(mst-largestWeight)

  end primAlgo

  def main(args: Array[String]): Unit =    
    val aa = br.readLine.split(' ')
    n = aa(0).toInt
    m = aa(1).toInt

    for i <- 0 to n do
      adjs += ArrayBuffer.empty

    for i <- 1 to m do
      val (a,b,c) = gets()
      adjs(a).addOne(b -> c)
      adjs(b).addOne(a -> c)
    
    primAlgo()

    br.close()

end p1647
