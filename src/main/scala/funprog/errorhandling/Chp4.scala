package funprog
import scala.{Option => _, Either => _, _}

object Chp4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(a1 => b map(b1 => f(a1, b1)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Some(x) :: xs => sequence(xs) map(x :: _)
    case None :: xs    => None
    case Nil           => Some(Nil)
  }

  def sequence1[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List.empty[A]))((x,y) => map2(x,y)(_ :: _))

  sequence(List(Some(3), Some(4), Some(5))) // => Some(List(3, 4, 5))
  sequence(List(None, Some(3), Some(4)))    // => None

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
//    case h :: t => f(h) flatMap(b => traverse(t)(f) map (b :: _))
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }
  assert(traverse(List("1", "2", "foo"))(s => Try(s.toInt)) == None)
  assert(traverse(List("1", "2", "3"))(s => Try(s.toInt)) == Some(List(1, 2, 3)))

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((a, b) => b flatMap (bb => f(a) map(_ :: bb)))
  assert(traverse2(List("1", "2", "foo"))(s => Try(s.toInt)) == None)
  assert(traverse2(List("1", "2", "3"))(s => Try(s.toInt)) == Some(List(1, 2, 3)))


  def sequence3[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
  sequence3(List(Some(3), Some(4), Some(5))) // => Some(List(3, 4, 5))
  sequence3(List(None, Some(3), Some(4)))    // => None

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }
}