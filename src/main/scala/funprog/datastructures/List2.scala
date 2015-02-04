package funprog.datastructures

sealed trait List2[+A]
case object Nil extends List2[Nothing]
case class Cons[+A](head: A, tail: List2[A]) extends List2[A]

object List2 {
  def apply[A](as: A*): List2[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def len[A](l: List2[A]): Int =
    foldLeft(l, 0)((b, a) => b + 1)

  def tail[A](l: List2[A]): List2[A] = l match {
    case Nil => throw new UnsupportedOperationException("tail on an empty list!")
    case Cons(_, t) => t
  }

  def setHead[A](l: List2[A], h: A): List2[A] = l match {
    case Nil => throw new UnsupportedOperationException("setHead on an empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List2[A], n: Int): List2[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  def dropWhile[A](l: List2[A], f: A => Boolean): List2[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List2[A]): List2[A] = l match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def map[A,B](l: List2[A])(f: A => B): List2[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def sum(ints: List2[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product(ds: List2[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def reverse[A](l: List2[A]): List2[A] =
    foldLeft(l, List2[A]())((a, b) => Cons(b, a))

  def append[A](a1: List2[A], a2: List2[A]): List2[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def filter[A](l: List2[A])(f: A => Boolean): List2[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t) => filter(t)(f)
  }

  def concat[A](l: List2[List2[A]]): List2[A] = foldRight(l, Nil:List2[A])(append)

  def flatMap[A,B](l: List2[A])(f: A => List2[B]): List2[B] =
    concat(map(l)(f))

  def zipWith[A,B,C](a: List2[A], b: List2[B])(f: (A,B) => C): List2[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def foldLeft[A,B](l: List2[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z, h))(f)
    }

  def foldRight[A,B](as: List2[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def startsWith[A](l: List2[A], prefix: List2[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  def hasSubsequence[A](l: List2[A], sub: List2[A]): Boolean = l match {
    case Nil => false
    case Cons(h, t) if startsWith(l, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}
