package funprog.errorhandling

import scala.{Option => _, Either => _, _}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

object OptionTest extends Specification with ScalaCheck {
  import Option._
  lazy val maybeThrow = throw new RuntimeException("expected not to call!")

  "mapping over none results in a none" in {
    None.map(a => a) mustEqual None
  }

  "mapping over a sum applies a function to the wrapped value" ! prop { i: Int =>
    Some(i).map(_ * 2) == Some(i * 2)
  }

  "getOrElse returns the else on a None" in {
    None getOrElse 1 mustEqual 1
  }

  "getOrElse lazily evaluates the else" in {
    Some(1) getOrElse maybeThrow mustEqual 1
    Some(1) getOrElse 2 mustEqual 1
  }

  "orElse provides an option alternative" in {
    None orElse Some(1) mustEqual Some(1)
    Some(1) orElse maybeThrow mustEqual Some(1)
    Some(1) orElse Some(2) mustEqual Some(1)
  }

  "flatMap maps a function that may also fail" ! prop { i: Int =>
    def doubleEvens(n: Int): Option[Int] =
      if (n % 2 == 0) Some(n * 2)
      else None

    Some(i).flatMap(n => doubleEvens(n)) == doubleEvens(i)
  }

  "filter on a None == None" in {
    None filter(_ => true) mustEqual None
  }

  "filter on a some returns some or none" in {
    Some(1) filter(_ < 2) mustEqual Some(1)
    Some(2) filter(_ < 2) mustEqual None
  }

  "Try" in {
    "is a function that will turn exceptions into Nones" in {
      Try(maybeThrow) mustEqual None
    }
    "is a function that will wrap a success in a Some" in {
      Try(2 * 2) mustEqual Some(4)
    }
  }

  "i can get the mean of a non empty sequence of doubles" in {
    mean(Seq[Double]()) mustEqual None
    mean(Seq(1.0, 2.0, 3.0)) mustEqual Some(2.0)
  }

  "i can get the variance of a non empty of doubles" in {
    variance(Seq[Double]()) mustEqual None
    variance(Seq(1.0, 2.0, 3.0)) mustEqual Some(2.toDouble / 3)
  }

  "map2 is a function that takes two optional values, and a function for operating on their Somes" in {
    map2(Some(1), None: Option[Int])(_ + _) mustEqual None
    map2(None: Option[Int], Some(1))(_ + _) mustEqual None
    map2(Some(1), Some(2))(_ + _) mustEqual Some(3)
  }

  "i can sequence a list of options into a optional list" in {
    sequence(List(None)) mustEqual None
    sequence(List(None, Some(1))) mustEqual None
    sequence(List(Some(1), None)) mustEqual None
    sequence(List(Some(1), Some(2))) mustEqual Some(List(1, 2))
  }

  val genSome: Gen[Option[Int]] = for (i <- arbitrary[Int]) yield Some(i)
  implicit def arbOption: Arbitrary[Option[Int]] = Arbitrary {
    Gen.oneOf(const(None), genSome)
  }
  "sequence can also be defined in terms of foldRight or traverse" ! prop {
    as: List[Option[Int]] =>
      val a, b, c = (sequence(as), sequence2(as), sequence3(as))
      a == b && b == c
  }

  "traverse walks a list, with a function that may fail, and gives an optional list" in {
    traverse(List[Int]())(_ => maybeThrow) mustEqual Some(List())
    traverse(List(1, 2, 3))(i => None) mustEqual None
    traverse(List(1, 2, 3))(i => Some(i * 2)) mustEqual Some(List(2, 4, 6))
  }

  "traverse can also be implemented in terms of foldRight" ! prop {
    as: List[Int] =>
      traverse(as)(i => Some(i * 2)) mustEqual traverse2(as)(i => Some(i * 2))
      traverse(as)(i => None) mustEqual traverse2(as)(i => None)
  }
}
