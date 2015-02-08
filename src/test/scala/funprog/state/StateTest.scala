package funprog.state

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object StateTest extends Specification with ScalaCheck {
  import State._
  sealed trait LightColor
  case object Green extends LightColor
  case object Yellow extends LightColor
  case object Red extends LightColor

  sealed trait Mode
  case object Off extends Mode
  case object Flashing extends Mode
  case object Solid extends Mode

  case class Signal(
    isOperational: Boolean,
    display: Map[LightColor, Mode]
  )

  type ->[A, B] = (A, B)
  object Signal {

    type SignalState[A] = State[Signal, A]

    val default = Signal(
      isOperational = false,
      display = Map(Red -> Flashing, Yellow -> Off, Green -> Off)
    )

    def enable: State[Signal, Boolean] =
      for {
        _ <- modify((s: Signal) => s.copy(isOperational = true))
        r <- get
      } yield r.isOperational

    def display(seq: Seq[LightColor -> Mode]): Signal => Signal =
      signal =>
        if (signal.isOperational) signal.copy(display = signal.display ++ seq.toMap)
        else default

    def change(seq: LightColor -> Mode*): State[Signal, Map[LightColor, Mode]] =
      for {
        _ <- modify(display(seq))
        s <- get
      } yield s.display
  }

  import Signal._
  val program = for {
    _ <- enable
    a <- get
    _ <- change(Red -> Off)
    b <- get
    _ <- change(Green -> Solid)
    c <- get
  } yield List(a, b, c)

  val states = program.run(default)._1
  "keeps track of stateful operations" in {
    val a = Signal(true, default.display)
    val b = Signal(true, default.display.updated(Red, Off))
    val c = Signal(
      true,
      default.display
        .updated(Red, Off)
        .updated(Green, Solid)
    )
    states must beLike {
      case List(a, b, c) => ok
      case _ => ko
    }
  }

  // TODO, this is not a great example
  "sequence" in {
    sequence[Int, Int](List(unit(1), unit(2), unit(3))).run(1)._1 mustEqual List(1, 2, 3)
  }
}
