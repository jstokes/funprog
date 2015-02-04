package funprog

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

object CandyTest extends Specification with ScalaCheck {
  import Candy._

  implicit def arbMachine: Arbitrary[Machine] = Arbitrary {
    for (candies <- arbitrary[Int]; coins <- arbitrary[Int]; locked <- arbitrary[Boolean])
    yield Machine(locked, candies, coins)
  }

  val withCandy = arbitrary[Machine] suchThat (_.candies > 0)

  "inserting a coin into a locked machine will cause it to unlock, if there is any candy left" ! Prop.forAll(withCandy) { (m: Machine) =>
    simulateMachine(List(Coin)).run(m)._2.locked must beFalse
  }.pendingUntilFixed
}
