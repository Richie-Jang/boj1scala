import scala.collection.mutable.ListBuffer

object BipartiteMatch {

    var adj = Array.empty[ListBuffer[Int]]
    var vs = Array.emptyBooleanArray
    var d = Array.emptyIntArray
    var n = 0

    def dfs(cur: Int): Boolean =        
        for i <- 0 until adj(cur).size do
            val t = adj(cur)(i)
            if vs(t) == false then
                vs(t) = true
                if d(t) == 0 || dfs(d(t)) then 
                    d(t) = cur
                    return true
        false

    @main def bipartite(): Unit =
        n = 3
        adj = Array.tabulate(n+1)(_ => ListBuffer.empty[Int])        
        d = Array.ofDim[Int](n+1)

        adj(1) += 1
        adj(1) += 2
        adj(1) += 3
        adj(2) += 1
        adj(3) += 2

        var count = 0
        for i <- 1 to n do
            vs = Array.ofDim[Boolean](n+1)
            if dfs(i) then count += 1

        println(s"$count matching is done")

        for i <- 1 to n do
            if d(i) != 0 then
                println(s"${d(i)} -> $i")


}