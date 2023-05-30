import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
object P1043:

  case class Party(persons: Set[Int], canTalk: Boolean)
  var n: Int = 0
  val sc = Scanner(System.in)
  var knownPersons = Set.empty[Int]
  val parties = ListBuffer.empty[Party]

  def checkParties(): Unit = 
    val queue = Queue.empty[Int]
    queue.addAll(knownPersons)
    val checked = Array.ofDim[Boolean](n+1)
    while !queue.isEmpty do
      //println(s"Current Queue: $queue")
      val cp = queue.dequeue
      if !checked(cp) then 
        checked(cp) = true      
        //println(s"$cp is dequeue")
        for pi <- parties.indices if parties(pi).canTalk == true do
          val party = parties(pi)
          if party.persons.contains(cp) then           
            parties(pi) = parties(pi).copy(canTalk = false)
            party.persons.filter(a => checked(a) != true).foreach(a => queue.enqueue(a))
      end if
    // counting
    val ans = parties.filter(a => a.canTalk).size
    println(ans)

  def main(args: Array[String]): Unit =
    n = sc.nextInt()
    val pn = sc.nextInt()      
    val kpcount = sc.nextInt()
    for i <- 0 until kpcount do
      knownPersons = knownPersons + sc.nextInt()
    for i <- 0 until pn do
      val cc = sc.nextInt()
      val ps = (for j <- 0 until cc yield sc.nextInt()).toSet      
      parties += Party(ps, true)        
    
    checkParties()

    sc.close()
end P1043