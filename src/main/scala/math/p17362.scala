import java.util.Scanner
object P17362:
    def main(args: Array[String]): Unit =
        val sc = new Scanner(System.in)
        val n = sc.nextInt()
        val res = 
            (n%8) match {
                case 1 => 1
                case 0 | 2 => 2
                case 3 | 7 => 3
                case 4 | 6 => 4
                case _ => 5
            }
        println(res)
        sc.close()
end P17362