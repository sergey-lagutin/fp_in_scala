package ch06

import State._

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)

  def update(i: Input)(s: Machine) =
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin - 1)

    }
}