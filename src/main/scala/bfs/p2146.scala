package bfs

import scala.io.StdIn
import scala.collection.mutable.Queue

object p2146 {
    var n: Int = 0
    val grid: Array[Array[Int]] = Array.ofDim[Int](100,100)
    val ds = List(
            -1 -> 0,
            1 -> 0,
            0 -> 1,
            0 -> -1
        )
    

    def fixedLandNum(): Unit = {
        var num = 2
        def bfs(y: Int, x: Int): Unit = {
            val q = Queue.empty[(Int, Int)]
            val vs = Array.ofDim[Boolean](n,n)
            q.enqueue(y -> x)
            grid(y)(x) = num
            vs(y)(x) = true
            while q.nonEmpty do
                val (cy, cx) = q.dequeue
                val ns = ds.map { (yy,xx) => (yy + cy) -> (xx + cx) }.filter { (yy, xx) => 0 <= yy && yy < n &&
                     0 <= xx && xx < n  && !vs(yy)(xx) && grid(yy)(xx) == 1 }
                for v <- ns do
                    grid(v._1)(v._2) = num
                    vs(v._1)(v._2) = true
                    q.enqueue(v)
                end for
            end while
        }

        for i <- 0 until n do
            for j <- 0 until n do
                val v = grid(i)(j)
                if v == 1 then
                    bfs(i,j)
                    num += 1
                end if
    }

    def searchShortDist(y: Int, x: Int, curNum: Int): Int = {        
        val q = Queue.empty[(Int,Int,Int)]
        val vs = Array.ofDim[Boolean](n,n)
        q.enqueue((y,x,0))
        vs(y)(x) = true
        var isChecked = false
        while q.nonEmpty do
            val (cy, cx, cc) = q.dequeue
            val ns = ds.map { (yy,xx) => (yy + cy) -> (xx + cx) }.filter { (yy, xx) => 0 <= yy && yy < n &&
                     0 <= xx && xx < n  && !vs(yy)(xx) }            
            for v <- ns do
                val g = grid(v._1)(v._2)
                if g == 0 then 
                    q.enqueue((v._1,v._2,cc+1))
                    vs(v._1)(v._2) = true                                                        
                else if g != curNum then
                    return cc
                end if
            end for            
        end while
        Int.MaxValue
    }
    
    def main(args: Array[String]): Unit = {
        n = StdIn.readLine.trim.toInt
        for i <- 0 until n do
            val ss = StdIn.readLine.trim.split("\\s+")
            for v <- 0 until ss.length do
                grid(i)(v) = ss(v).toInt
        end for

        fixedLandNum()

        var res = Int.MaxValue

        for i <- 0 until n do
            for j <- 0 until n do
                val g = grid(i)(j)
                if g > 1 then
                    val ff = searchShortDist(i,j,g) 
                    //printf("[%d,%d]=%d\n", i,j, ff)
                    res = scala.math.min(ff, res)

        println(res)

    }


    def printGrid(): Unit = {
        for i <- 0 until n do
            for j <- 0 until n do
                printf("%d ", grid(i)(j))
            end for
            println()
        end for
    }
}