package funprog.errorhandling

import scala.{Option => _, Either => _, _}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

object OptionTest extends Specification with ScalaCheck {
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
}
