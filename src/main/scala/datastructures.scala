package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail(l: List[B]): List[A] = l match {
    case Nil => throw new Exception("Empty lists do not have tails")
    case Cons(_, xs) => xs
  }

  def setHead(l: list[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, 0) => l
    case (Nil, -1) => throw new Exception("Cannot remove more elements than the list contains")
    case (_,_) =>   drop(l, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case f(Cons(_, xs)) => dropWhile(xs, f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case x :: _ :: Nil => Cons(x, Nil)
    case Cons(x, xs) => Cons(x, init(xs))
  }



}