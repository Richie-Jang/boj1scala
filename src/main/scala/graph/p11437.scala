package graph

import java.io.BufferedReader
import java.io.InputStreamReader
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

object p11437 {
    var br: BufferedReader = _
    var graph = Array.empty[ArrayBuffer[Int]]
    val inps = ArrayBuffer.empty[(Int,Int)]
    var n = 0
    var m = 0
    inline def getvs: Array[String] =
        val s = br.readLine.trim.split("\\s+")
        s

    def solve(): Unit = {
        val root = 1
        val tree = Array.ofDim[Int](n+1)
        val vs = Array.ofDim[Boolean](n+1)
        val parents = Array.tabulate(n+1)(a => a)
        def dfs(curNode: Int, depth: Int): Unit = {
            if curNode > n then ()
            else
                tree(curNode) = depth
                vs(curNode) = true
                val nexts = graph(curNode).filter(a => !vs(a))
                //println(s"Cur: $curNode Depth: $depth => $nexts")                
                nexts.foreach{ a => 
                    parents(a) = curNode
                    dfs(a, depth+1) 
                }
        }        
        dfs(1, 0)
        val sb = mutable.StringBuilder()
        inline def addResult(s: Int): Unit = sb.append(s).append(System.lineSeparator)
        def lca(a: Int, b: Int): Unit = {
            if a == b then addResult(a)
            else
                val a1 = tree(a)
                val b1 = tree(b)
                if a1 < b1 then lca(a, parents(b)) else lca(parents(a), b)
        }            
        inps.foreach((a,b) => lca(a,b))
        print(sb.toString())
    }

    def main(args: Array[String]): Unit = {
        br = new BufferedReader(new InputStreamReader(System.in))
        n = br.readLine.trim.toInt
        graph = Array.tabulate(n+1)(_ => ArrayBuffer.empty)
        for i <- 1 until n do
            val Array(a,b) = getvs.map(_.toInt)
            graph(a) += b            
            graph(b) += a
        m = br.readLine.trim.toInt
        for i <- 1 to m do
            val Array(a,b) = getvs
            inps += a.toInt -> b.toInt        
        solve()        
        br.close()
    }
}