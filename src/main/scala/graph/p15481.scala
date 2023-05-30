package graph

import java.util.Scanner
import java.io.BufferedInputStream
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable

object p15481 {

    val sc = new Scanner(BufferedInputStream(System.in))    
    var n = 0
    var m = 0
    var data = ListBuffer.empty[(Int,Int,Long)]
    var parents = Array.emptyIntArray

    def findParent(p: Int): Int = {
        if parents(p) == p then p
        else        
            findParent(parents(p))
    }        

    def union (a: Int, b: Int): Unit = {
        val pa = findParent(a)
        val pb = findParent(b)
        if pa < pb then parents(pb) = pa else parents(pa) = pb
    }
    
    def computeMST(inp: (Int,Int,Long)): Long = {
        parents = Array.ofDim[Int](n+1)
        val visits = HashSet.empty[(Int,Int)]
        var count = 0L
        for i <- 1 to n do parents(i) = i
        visits.add(inp._1 -> inp._2)        
        union(inp._1, inp._2)
        count += inp._3
        for (np1, np2,ww) <- data if !visits.contains(np1 -> np2) do
            // check circle
            val npp1 = findParent(np1)
            val npp2 = findParent(np2)
            if npp1 != npp2 then 
                count += ww
                visits.add(np1 -> np2)                
                union(np1, np2)

        count
    }

    // main method
    def main(args: Array[String]): Unit = {

        n = sc.nextInt()
        m = sc.nextInt()
        val inputs = ListBuffer.empty[(Int,Int,Long)]
        for i <- 1 to m do
            val p1 = sc.nextInt()
            val p2 = sc.nextInt()
            val w = sc.nextLong()
            data += ((p1,p2,w))
            inputs += ((p1,p2,w))

        sc.close()

        // sort
        data = data.sortBy(_._3)
        val sb = mutable.StringBuilder()
        for (a,b,c) <- inputs do
            sb.append(computeMST(a,b,c)).append(System.lineSeparator)           
        print(sb.toString())
        
    }
}