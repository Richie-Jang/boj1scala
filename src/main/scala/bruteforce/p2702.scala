package bruteforce

import scala.io.StdIn
import scala.collection.mutable

object p2702 {

    var n = 0
    
    def gets: (Int,Int) = {
        val g = StdIn.readLine.trim.split("\\s+")
        g(0).toInt -> g(1).toInt
    }

    def solve(sb: mutable.StringBuilder): Unit = {        
        def gcd (a: Int, b: Int): Int = {
            if (b == 0) return a                        
            gcd (b, a % b)
        }
        
        val (a, b) = gets

        val a1 = gcd (a, b)
        val b1 = a * b / a1
        sb.append(s"$b1 $a1").append(System.lineSeparator)
    }

    def main(args: Array[String]): Unit = {
        n = StdIn.readLine.trim.toInt

        val sb = new mutable.StringBuilder()
        for (i <- 1 to n) {
            solve(sb)
        }
        print(sb.toString)
    }


}