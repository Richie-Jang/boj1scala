package graph

import scala.util.chaining.*
import scala.io.StdIn
import scala.collection.mutable.Queue

object p2636 {

    var n, m = 0
    var grid = Array.empty[Array[Int]]    

    def printGrid: Unit = {
        grid.foreach{a =>
            println(a.mkString(" "))
        }
    }

    def solve: Unit = {

        val ds = Array(
            -1 -> 0,
            1 -> 0,
            0 -> -1,
            0 -> 1            
        )

        def checkExists: Boolean = {            
            for y <- grid.indices do
                for x <- grid(0).indices do
                    if grid(y)(x) >= 1 then return true
            return false
        }

        def makeHolesAndGetCount: Int = {
            var count = 0
            val visits = Array.ofDim[Boolean](n,m)

            def getDirs(a: Int, b: Int): Array[(Int,Int)] = {
                ds.map { (a1,b1) => (a1+a) -> (b1 + b) }
                    .filter { (a1,b1) => 
                        a1 >= 0 && a1 < m && b1 >= 0 && b1 < n && !visits(b1)(a1)
                    }
            }

            val q = Queue.empty[(Int,Int)]
            // add outside
            for i <- grid.indices do
                val a1 = 0 -> i
                val a2 = (m-1) -> i
                q.enqueueAll(Seq(a1,a2))
                visits(i)(0) = true
                visits(i)(m-1) = true
            end for
            // outside check
            while q.nonEmpty do
                val (x,y) = q.dequeue
                getDirs(x,y).foreach { (x,y) =>
                    if grid(y)(x) == 1 then 
                        count += 1
                        grid(y)(x) = 2
                    else if grid(y)(x) == 0 then 
                        q.enqueue(x -> y)
                        visits(y)(x) = true
                }
            end while     
            count       
        }

        def meltDown: Unit = {
            for y <- 0 until n do
                for x <- 0 until m do
                    if grid(y)(x) == 2 then grid(y)(x) = 0
        }
        
        var cheeseCount = makeHolesAndGetCount
        var timer = 0
                        
        while checkExists do
            // melt            
            meltDown
            timer += 1
            val g = makeHolesAndGetCount
            if g > 0 then cheeseCount = g
        end while

        println(timer)
        println(cheeseCount)
    }

    def main(args: Array[String]): Unit = {
        StdIn.readLine.trim.split("\\s+").tap{a =>
            n = a(0).toInt
            m = a(1).toInt
        }
        grid = Array.ofDim(n,m)        
        for i <- 0 until n do
            grid.update(i, StdIn.readLine.trim.split("\\s+").map(_.toInt))
        
        solve
    }
}