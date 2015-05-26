package chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

object Machine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(initialState => {
      val machine = inputs.foldLeft(initialState)((acc,input) => acc.operate(input) )
      (machine.coins -> machine.candies, machine)
    })

}

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  private def operate(input : Input) : Machine = {
    input match {
      case Coin if locked => Machine(false, candies, coins + 1)
      case Turn if !(locked) && candies > 0 => Machine(true, candies - 1, coins)
      case _ => this
    }
  }

}