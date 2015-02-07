package funprog.state

import State._

case class Machine(locked: Boolean, candies: Int, coins: Int)
sealed trait Input
case object Coin extends Input
case object Turn extends Input

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
