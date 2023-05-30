package dynamic

import scala.util.chaining.*
@main def p2225_Main(): Unit = {
    
    var (n,k) = 0 -> 0
    io.StdIn.readLine().split("\\s+").map(_.toInt).tap { a => 
        n = a(0)
        k = a(1)
    }    

    val dp = Array.ofDim[Int](n+1, k+1)

    for i <- 0 to k do dp(0)(i) = 1
    for 
        i <- 1 to n
        j <- 1 to k
    do {        
        dp(i)(j) = (dp(i)(j-1) + dp(i-1)(j)) % 1_000_000_000        
    }
    
    println(dp(n)(k))

}