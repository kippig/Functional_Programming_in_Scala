import scala.annotation.tailrec

object Fibonacci {

  def nth(n:Int):BigInt = {
    @tailrec def recurse(a: BigInt, b: BigInt, descender:Int): (BigInt, BigInt, Int) = {
      if (descender == -1)
        (a, b, descender)
      else if (descender == 0)
        (b, b + a, descender)
      else
        recurse(b, a + b, descender - 1)
    }

    recurse(0,1,n-2)._1
  }

  def main(args: Array[String]): Unit = {
    println(nth(args(0).toInt))
  }
}