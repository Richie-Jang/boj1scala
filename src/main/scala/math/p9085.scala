import java.util.Scanner
import scala.collection.mutable
object P9085:
    var t  = 0    
    val sb = new mutable.StringBuilder()

    def solve(sc: Scanner): Unit =
        val n = sc.nextInt()
        val arr = Array.ofDim[Int](n)
        for i <- 0 until n do
            arr(i) = sc.nextInt()
        sb.append(arr.sum).append(System.lineSeparator)

    def main(args: Array[String]): Unit =
        val sc = new Scanner(System.in)
        t = sc.nextInt()
        for i <- 0 until t do
            solve(sc)        
        sc.close()
        println(sb.toString())
end P9085