package funprog

case class Machine(locked: Boolean, candies: Int, coins: Int) 

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

