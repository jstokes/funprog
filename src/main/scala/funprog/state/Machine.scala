package funprog.state

import State._

case class Machine(locked: Boolean, candies: Int, coins: Int)
sealed trait Input
case object Coin extends Input
case object Turn extends Input

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modify(onInput(input))))
    s <- get
  } yield (s.coins, s.candies)

  def onInput(input: Input): Machine => Machine =
    machine => (machine, input) match {
      case (Machine(_, candy, coin), _) if candy <= 0 => machine
      case (Machine(true, candy, coin), Coin) => Machine(false, candy, coin + 1)
      case (Machine(false, candy, coin), Turn) => Machine(true, candy - 1, coin)
      case (Machine(true, candy, coin), Turn) => machine
      case (Machine(false, candy, coin), Coin) => machine
    }
}
