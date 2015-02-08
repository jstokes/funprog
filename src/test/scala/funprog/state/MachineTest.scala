package funprog.state

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

object CandyTest extends Specification with ScalaCheck {
  import Candy._

  implicit def arbMachine: Arbitrary[Machine] = Arbitrary {
    for (candies <- posNum[Int]; coins <- posNum[Int]; locked <- arbitrary[Boolean])
      yield Machine(locked, candies, coins)
  }

  val arbM = arbitrary[Machine]
  val locked = arbM map (m => m.copy(locked = true))
  val unlocked = arbM map (m => m.copy(locked = false))
  val noCandy = arbM map (m => m.copy(candies = 0))
  val withCandy: Machine => Boolean = _.candies > 0
  val arbInput = Gen.listOf(Gen.oneOf(Coin, Turn))

  "inserting a coin into a locked machine will cause it to unlock, if there is any candy left" !
    Prop.forAll(locked filter withCandy) { m: Machine =>
      simulateMachine(List(Coin)).run(m)._2.locked must beFalse
    }

  "turning the knob on an unlocked machine will cause it to dispense candy and become locked" !
    Prop.forAll(unlocked) { m: Machine =>
      val result = simulateMachine(List(Turn)).run(m)._2
      result.locked == true && result.candies == m.candies - 1
    }

  "turning the knob on a locked machine does nothing" ! Prop.forAll(locked) { m: Machine =>
    simulateMachine(List(Turn)).run(m)._2 == m
  }


  "inserting a coin into an unlocked machine does nothing" ! Prop.forAll(unlocked) { m: Machine =>
    val result = simulateMachine(List(Coin)).run(m)._2
    result == m
  }

  "a machine that is out of candy ignores all inputs" ! Prop.forAll(noCandy, arbInput) {
    (m: Machine, inputs: List[Input]) =>
      simulateMachine(inputs).run(m)._2 == m
  }
}
