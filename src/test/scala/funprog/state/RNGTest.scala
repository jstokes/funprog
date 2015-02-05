package funprog.state

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

object RNGTest extends Specification with ScalaCheck {
  import RNG._

  implicit def arbRNG: Arbitrary[RNG] = Arbitrary(arbitrary[Long].map(x => Simple(x)))

  "randon number generator" should {
    "generate non negative integers" ! prop { rng: RNG => nonNegativeInt(rng)._1 must be >= 0 }

    "generate a list of integers" ! prop { rng: RNG => ints(10)(rng)._1.size mustEqual 10 }

    "generate doubles using map" ! prop { rng: RNG =>
      double(rng)._1 must beCloseTo(double2(rng)._1 within 5.significantFigures)
    }

    "sequence should turn a list of randoms into a random list" ! prop { rng: RNG =>
      sequence(List(unit(1), unit(2), unit(3)))(rng)._1 mustEqual List(1, 2, 3)
    }

    "map and mapWithFM are functionally equivalent" ! prop { rng: RNG =>
      RNG.map(int2)(_ + 1)(rng) mustEqual RNG.mapWithFM(int2)(_ + 1)(rng)
    }

    "map2 and map2WithFM are functionally equavalent" ! prop { (rng: RNG) =>
      map2(int2, int2)(_ + _)(rng) mustEqual map2WithFM(int2, int2)(_ + _)(rng)
    }

    "nonNegativeLessThan" ! forAll(posNum[Int], arbRNG.arbitrary) { (n: Int, rng: RNG) =>
      val next = nonNegativeLessThan(n)(rng)._1
      (next >= 0) && (next < n)
    }

    "generate unique values for unique seeds" ! prop { l1: Long =>
      val rng1 = Simple(l1)
      val rng2 = Simple(l1 + 1)

      doubleInt(rng1) mustNotEqual doubleInt(rng2)
      double3(rng1) mustNotEqual double3(rng2)
      ints(10)(rng1) mustNotEqual ints(10)(rng2)
    }
  }
}
