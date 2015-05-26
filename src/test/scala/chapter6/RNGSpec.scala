package chapter6

import org.specs2.mutable.Specification

/**
 * User: mauricio
 * Date: 5/18/15
 * Time: 11:49 PM
 */
class RNGSpec extends Specification {

  val generator = SimpleRNG(10)

  def simulateMachine(machine : Machine, inputs : Input*) : Machine = {
    val ((coins,candies),result) = Machine.simulateMachine(inputs.toList).run(machine)
    return result
  }

  "generator" should {

    "produce a list of values" in {
      val (list,next) = generator.ints(10)(generator)
      list.size === 10
    }

    "machine simulator with normal operation" in {
      val machine = Machine(true, 5, 10)

      val result = simulateMachine(machine, Coin, Turn, Coin, Turn, Coin, Turn)

      result.coins === 13
      result.candies === 2
      result.locked must beTrue
    }

    "turn a locked machine and nothing happens" in {
      val result = simulateMachine(Machine(true, 1, 1), Turn)

      result.coins === 1
      result.candies === 1
      result.locked must beTrue
    }

    "should insert a coing to an unlocked machine and nothing happens" in {
      val result = simulateMachine(Machine(false, 1, 1), Coin)

      result.coins === 1
      result.candies === 1
      result.locked must beFalse
    }

  }

}
