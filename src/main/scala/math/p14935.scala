package math

import scala.io.StdIn

object p14935 {

    def checkFA(s: String): Unit = {
        val f1 = (s(0) - '0').toInt
        val len = s.length
        val r = (f1 * len).toString
        if r == s then println("FA")
        else
            checkFA(r)    
    }

    def main(args: Array[String]): Unit = {
        //val x = StdIn.readLine.trim        
        val x = "941239209321235432122"
        checkFA(x)
    }
}