package chapter6

import org.specs2.mutable.Specification

/**
 * User: mauricio
 * Date: 5/18/15
 * Time: 11:49 PM
 */
class RNGSpec extends Specification {

  val generator = SimpleRNG(10)

  "generator" should {

    "produce a list of values" in {
      val (list,next) = generator.ints(10)(generator)
      list.size === 10
    }

  }

}
