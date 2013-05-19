package chapter2

import org.specs2.mutable.Specification

/**
 * User: Maurício Linhares
 * Date: 8/11/12
 * Time: 12:15 AM
 */

class ExerciseSpecs extends Specification {


  "exercises" should {

    "correctly calculate the absolute value for a negative number" in {
      Absolute.abs( -45 ) must beEqualTo( 45 )
    }

    "correctly calculate the absolute value for a positive number" in {
      Absolute.abs( 10 ) must beEqualTo( 10 )
    }

    "say that two is even" in {
      OddEven.isEven(2) must beTrue
    }

    "say that three is odd" in {
      OddEven.isOdd(3) must beTrue
    }

    "say that a number is even with predicates" in {
      HigherOrder.isEven( 2 ) must beTrue
    }

    "say that it is divisable by 3 and five" in {
      HigherOrder.isDivisableByThreeAndFive(15) must beTrue
      HigherOrder.isDivisableByThreeAndFive(10) must beFalse
    }

    "say that it is divisable by 3 or five" in {
      HigherOrder.isDivisableByThreeOrFive(15) must beTrue
      HigherOrder.isDivisableByThreeOrFive(10) must beTrue
      HigherOrder.isDivisableByThreeOrFive(9) must beTrue
    }

  }

}
