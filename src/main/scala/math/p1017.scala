import java.util.Scanner
import scala.collection.mutable.ListBuffer
import java.util.LinkedList
import scala.collection.mutable.TreeSet
import java.io.BufferedInputStream

object P1017:
  val MAX = 51
  val sc = new Scanner(new BufferedInputStream(System.in))
  var n = 0
  var arr = Array.emptyIntArray
  var firstNum = 0
  val primes = Array.ofDim[Boolean](2002)
  var adjs = Array.empty[ListBuffer[Int]]
  val vs = Array.ofDim[Boolean](MAX)
  val rArrForLeftIndex = Array.ofDim[Int](MAX)

  def updatePrimes(): Unit =
    def isPrime(k: Int, st: Int): Boolean =
      if k % 2 == 0 then false      
      else if st*st > k then 
        primes(k) = true
        true
      else
        if k % st == 0 then false
        else isPrime(k, st+2)
    primes(2) = true
    for i <- 3 to 2001 do isPrime(i, 3)    

  def solve(): Unit =
    adjs = Array.tabulate(n)(_ => ListBuffer.empty)
    // create adjs
    for i <- 0 until n do
      for j <- 0 until n do
        if i != j then
          if primes(arr(i)+arr(j)) then adjs(i) += j

    val firstSelections = TreeSet.empty[Int]
        
    def dfs (cur: Int): Boolean = 
      if vs(cur) then return false
      vs(cur) = true
      for i <- 0 until adjs(cur).size do
        val t = adjs(cur)(i)                
        if rArrForLeftIndex(t) == -1 || dfs(rArrForLeftIndex(t)) then
          rArrForLeftIndex(t) = cur
          return true
      false

    def subCheck(cur: Int, acc: Int): Int =
      if cur >= n then acc
      else
        vs.indices.foreach(aa => vs(aa) = false)
        vs(0) = true
        if dfs(cur) then subCheck(cur+1, acc+1) else acc

    for i <- 0 until adjs(0).size do
      rArrForLeftIndex.indices.foreach(aa => rArrForLeftIndex(aa) = -1)
      rArrForLeftIndex(adjs(0)(i)) = 0
      var count = 1
      count += subCheck(1, 0)
      
      if count == n then firstSelections.addOne(arr(adjs(0)(i)))
    
    // output
    if firstSelections.isEmpty then println("-1")
    else
      println(firstSelections.mkString(" "))

  end solve

  def main(args: Array[String]): Unit =
    n = sc.nextInt()    
    arr = Array.ofDim[Int](n)        
    for i <- 0 until n do
      arr(i) = sc.nextInt()    
    firstNum = arr(0)
    updatePrimes()    
    solve()
    sc.close()

end P1017