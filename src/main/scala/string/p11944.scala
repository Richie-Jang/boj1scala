package string

import scala.io.StdIn
import scala.util.chaining.*
import scala.collection.mutable
object p11944 {

    var n, m = 0
    var len = 0     

    def main(args: Array[String]): Unit = {
        StdIn.readLine.trim.split("\\s+").tap { a => len = a(0).length; n = a(0).toInt; m = a(1).toInt }
        
        val len1 = len*n
        val smallLen = scala.math.min(len1, m)

        val count = smallLen / len
        val remained = smallLen % len

        val sb = mutable.StringBuilder()
        val ns = n.toString
        for (i <- 1 to count) {
            sb.append(ns)
        }
        if (remained > 0) {
            for (i <- 1 to remained) {
                sb.append(ns(i-1))
            }
        }
        println(sb.toString)
    }

}