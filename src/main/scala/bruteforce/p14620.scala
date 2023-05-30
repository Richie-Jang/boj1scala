package bruteforce

import scala.io.StdIn

object p14620 {

    val inps = """6
1 0 2 3 3 4
1 1 1 1 1 1
0 0 1 1 1 1
3 9 9 0 1 99
9 11 3 1 0 3
12 3 0 0 0 1""".stripMargin

    var ground = Array.empty[Array[Int]]  
    var n = 0  

    def buildGnd(c: Int): Unit = {
        ground = Array.tabulate(c)(_ => Array.ofDim[Int](c))        
        for (y <- 0 until c) {
            val g = StdIn.readLine.split("\\s+")
            for (x <- 0 until c) {
                ground(y)(x) = g(x).toInt
            }
        }
    }

    def solve(): Unit = {
        var minValue = Int.MaxValue
        val flowers = List(
            0 -> 0,
            -1 -> 0,
            1 -> 0,
            0 -> -1,
            0 -> 1
        )

        val visited = Array.ofDim[Boolean](n,n)

        def makeFlowers(y: Int, x: Int): List[(Int,Int)] = {
            flowers.map { (a,b) => 
                (a+y) -> (b+x)
            }
        }
        
        def check(y: Int, x: Int): Boolean = {
            val overlapp = 
                makeFlowers(y,x).exists{ (a,b) => 
                    a < 0 || b < 0 || a > n-1 || b > n-1 || visited(a)(b)
                }     
            !overlapp
        }        

        def computeCost(y: Int, x: Int): Int = {
            makeFlowers(y,x).foldLeft(0){ (a,b) => a + ground(b._1)(b._2)}
        }

        def dfs(curX: Int, count: Int, cost: Int): Unit = {
            if (count == 3) {
                minValue = scala.math.min(minValue, cost)
                return ()
            }

            for {
                y <- 1 to n-1
                x <- curX to n-1                
            } {
                if (check(y,x)) {                    
                    makeFlowers(y,x).foreach((a,b) => visited(a)(b) = true)
                    dfs(x, count+1, cost + computeCost(y,x))
                    makeFlowers(y,x).foreach((a,b) => visited(a)(b) = false)
                }
            }
        }

        dfs(1, 0, 0)
        println(minValue)
    }

    def main(args: Array[String]): Unit = {
        //val ls = inps.split("\n").map(_.trim)
        n = StdIn.readLine.trim.toInt    
        buildGnd(n)
        solve()        
    }
}