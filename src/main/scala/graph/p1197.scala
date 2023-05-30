package graph

import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

object p1197 {
    case class Node(p1: Int, p2: Int, cost: Long)
    val sc = new Scanner(System.in)
    var v = 0
    var m = 0
    var adjs = ListBuffer.empty[Node]    
    var weights = 0L
    var nodeCount = 0
    var parents = Array.emptyIntArray

    def find(cur: Int): Int = {
        if cur == parents(cur) then cur
        else 
            parents(cur) = find(parents(cur))
            parents(cur)
    }

    def union(a: Int, b: Int): Boolean = {
        val aP = find(a)
        val bP = find(b)
        if aP == bP then false
        else
            parents(aP) = bP
            true
    }

    def main(args: Array[String]): Unit = {
        v = sc.nextInt()
        parents = Array.ofDim(v+1)
        m = sc.nextInt()        
        for i <- 1 to m do
            val a = sc.nextInt()
            val b = sc.nextInt()
            val c = sc.nextInt()
            adjs += Node(a,b,c)
        sc.close()
        // sorted
        adjs = adjs.sortBy(a => a.cost)            
        for i <- 1 to v do
            parents(i) = i

        // do
        for nn <- adjs do
            if union(nn.p1, nn.p2) then 
                weights += nn.cost
                nodeCount += 1
            if nodeCount == v-1 then 
                println(weights)        
                return
    }
}