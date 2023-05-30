package graph

import java.util.Scanner
import java.io.BufferedInputStream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue

object P1916 {    
    val sc = new Scanner(BufferedInputStream(System.in))
    var n = 0
    var m = 0
    var startpos = 0
    var endpos = 0
    var adjs = Array.empty[ListBuffer[(Int,Int)]]  // pos, cost
    given cmp: Ordering[(Int,Int)] with
        def compare(o1: (Int,Int), o2: (Int,Int)): Int =
            (o1._2.compareTo(o2._2)) * -1    
    val queue = PriorityQueue.empty[(Int,Int)]
    var dist = Array.emptyIntArray

    def solve(): Unit = {    
        // start
        dist(startpos) = 0
        queue.enqueue(startpos -> 0)
        
        while !queue.isEmpty do
            val (cur, cost) = queue.dequeue
            if dist(cur) <= cost then 
                for next <- adjs(cur) do
                    val (npos, nCost) = next
                    val newCost = cost + nCost
                    if newCost < dist(npos) then
                        dist(npos) = newCost
                        queue.enqueue(npos -> newCost)
            
        end while
        println(dist(endpos))            
    }

    def main(args: Array[String]): Unit = {
        n = sc.nextInt()
        m = sc.nextInt()
        adjs = Array.tabulate(n+1)(_ => ListBuffer.empty)        
        dist = Array.tabulate(n+1)(_ => Int.MaxValue)        
        for i <- 0 until m do
            val p1 = sc.nextInt()
            val p2 = sc.nextInt()
            val pay = sc.nextInt()
            adjs(p1) += (p2 -> pay)
        
        startpos = sc.nextInt()
        endpos = sc.nextInt()
        sc.close()
        solve()        
    }
}