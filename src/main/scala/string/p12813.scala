package string

import scala.io.StdIn
import scala.collection.mutable

object p12813 {

    def main(args: Array[String]): Unit = {

        val a = StdIn.readLine.trim
        val b = StdIn.readLine.trim

        val alen = a.length
        val blen = b.length
        val len = scala.math.max(alen, blen)

        val res1 = Array.ofDim[Char](len)
        val res2 = Array.ofDim[Char](len)
        val res3 = Array.ofDim[Char](len)
        val res4 = Array.ofDim[Char](len)
        val res5 = Array.ofDim[Char](len)
                
        for i <- 0 until len do
            if i < alen && i < blen then
                // and
                (a(i), b(i)) match {
                    case ('1','1') => res1(i) = '1'
                    case _ => res1(i) = '0'
                }
                // or
                (a(i), b(i)) match {
                    case ('1','0') | ('0','1') | ('1','1') => res2(i) = '1'
                    case _ => res2(i) = '0'
                }
                // xor
                (a(i), b(i)) match {
                    case ('1','0') | ('0','1') => res3(i) = '1'
                    case _ => res3(i) = '0'
                }
            end if
            if i < alen then 
                a(i) match {
                    case '1' => res4(i) = '0'
                    case _ => res4(i) = '1'
                }
            end if
            if i < blen then
                b(i) match {
                    case '1' => res5(i) = '0'
                    case _ => res5(i) = '1'
                }
            end if
        end for

        println(new String(res1))
        println(new String(res2))
        println(new String(res3))
        println(new String(res4).trim)
        println(new String(res5).trim)
    }
}