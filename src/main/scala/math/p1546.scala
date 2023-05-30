package math

@main def p1546_Main(): Unit = {
    val n = io.StdIn.readLine().toInt

    var (maxValue, len) = 0 -> 0
    val arr = io.StdIn.readLine().split("\\s+").map { a => 
        len += 1
        val b = a.toInt
        if maxValue < b then maxValue = b
        b
    }    

    val get2DigitDouble = {(v: Double) =>
        val v1 = v * 100.0    
        val v2 = scala.math.floor(v1)
        v2 / 100.0
    }    

    var newAvg = 0.0
    for i <- arr do {
        val vv = get2DigitDouble(i * 1.0 / maxValue * 100.0)
        newAvg += vv
    }

    newAvg = newAvg / len
    println(String.format("%f", newAvg))
    
}