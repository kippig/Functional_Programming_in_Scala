import scala.annotation.tailrec

object Chapter2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(args, (x: String, y: String) => y > x))
  }
}
