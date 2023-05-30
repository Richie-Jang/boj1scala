package LIS

import scala.math

@main def Learn1: Unit = {

    val arr = Array(10,20,10,30,20,50)
    // set dp 1
    val dp = Array.fill(arr.length)(1)

    for 
        i <- 0 until arr.length
        j <- 0 until i
    do
        val a = arr(i)
        val b = arr(j)
        
        println(s"i:$i j:$j => ${arr(i)} ${arr(j)} ${if a > b then "Count 1" else "" }")
        if a > b then dp(i) = math.max(dp(i), dp(j)+1)
    
    println(dp.mkString(" "))

}
