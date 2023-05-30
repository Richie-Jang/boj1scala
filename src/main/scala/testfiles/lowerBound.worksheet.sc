val arr = Array(1, 3, 3, 4, 5, 5, 6, 7, 8, 8, 10)

def lowerBound(st: Int, ed: Int, target: Int): Int = {

    var left = st
    var right = ed
    while left < right do
        val m = (left + right) / 2
        if arr(m) < target then 
            left = m + 1
        else
            right = m
    end while

    right

}

def upperBound(st: Int, ed: Int, target: Int): Int = {
    var left = st
    var right = ed
    while left < right do
        val m = (left + right) / 2
        if arr(m) <= target then 
            left = m + 1
        else
            right = m
    end while

    right
}

val r1 = lowerBound(0, arr.size-1, 8)
println(r1)

val r2 = upperBound(0, arr.size-1, 8)