package chapter2

import org.specs2.mutable.Specification

/**
 * User: mauricio
 * Date: 5/21/13
 * Time: 10:42 PM
 */
class Chapter2Specs extends Specification {

  val sortFunction = (x : Int, y : Int) => x > y

  "chapter 2" should {

    "correctly calculate the fib" in {

      // 0-1-1-2-3-5-8-13-21-34-55

      Chapter2.fib(0) === 0
      Chapter2.fib(1) === 1
      Chapter2.fib(2) === 1
      Chapter2.fib(3) === 2
      Chapter2.fib(4) === 3
      Chapter2.fib(5) === 5
      Chapter2.fib(6) === 8

    }

    "correctly say an array is not sorted" in {
      Chapter2.isSorted(Array(1,2,3,0), sortFunction) must beFalse
      Chapter2.isSorted(Array(1,3,2,4), sortFunction) must beFalse
    }

    "correcty say that an array is sorted" in {
      Chapter2.isSorted(Array(1,2,3,4), sortFunction) must beTrue
      Chapter2.isSorted(Array(4,5,5,6,7), sortFunction) must beTrue
    }

  }

}
