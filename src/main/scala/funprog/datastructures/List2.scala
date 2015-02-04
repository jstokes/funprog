package funprog.datastructures

sealed trait List2[+A]
case object Nil extends List2[Nothing]
case class Cons[+A](head: A, tail: List2[A]) extends List2[A]

object List2 {
  def apply[A](as: A*): List2[A] = ???

  def len[A](l: List2[A]): Int = ???

  def tail[A](l: List2[A]): List2[A] = ???

  def setHead[A](l: List2[A], h: A): List2[A] = ???

  def drop[A](l: List2[A], n: Int): List2[A] = ???

  def dropWhile[A](l: List2[A], f: A => Boolean): List2[A] = ???

  def reverse[A](l: List2[A]): List2[A] = ???

  def init[A](l: List2[A]): List2[A] = ???

  def foldLeft[A,B](l: List2[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List2[A])(f: A => B): List2[B] = ???

  def sum(ints: List2[Int]): Int = ???

  def product(ds: List2[Double]): Double = ???

  def append[A](a1: List2[A], a2: List2[A]): List2[A] = ???

  def foldRight[A,B](as: List2[A], z: B)(f: (A, B) => B): B = ???

  def concat[A](l: List2[List2[A]]): List2[A] = ???

  def filter[A](l: List2[A])(f: A => Boolean): List2[A] = ???

  def flatMap[A,B](l: List2[A])(f: A => List2[B]): List2[B] = ???

  def zipWith[A,B,C](a: List2[A], b: List2[B])(f: (A,B) => C): List2[C] = ???

  def hasSubsequence[A](l: List2[A], sub: List2[A]): Boolean = ???
}
