package chapter5

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "streams" should {

    "pull the head from a non-empty stream" in {
      Stream(1, 2, 3).headOption.get === 1
    }

    "returns none for the head if the stream is empty" in {
      Stream.empty[Int].headOption === None
    }

    "turn a stream into a list" in {
      Stream(1, 2, 3).toList === List(3, 2, 1)
    }

    "take N elements from a stream" in {
      Stream(1, 2, 3, 4).take(2).toList === List(2, 1)
    }

    "take more elements than the list has" in {
      Stream(1, 2).take(3).toList === List(2, 1)
    }

    "take elements from an empty list" in {
      Stream.empty[Int].take(5).toList must beEmpty
    }

    "drops N elements from the stream" in {
      Stream(1, 2, 3).drop(2).toList === List(3)
    }

    "takes numbers while they're less than 3" in {
      Stream(1, 2, 3, 4).takeWhile(_ < 3).toList === List(2, 1)
    }

    "does not take anything if the list is empty" in {
      Stream.empty[Int].takeWhile(_ < 3).toList must beEmpty
    }

    "checks if an item exists" in {
      Stream(1, 2, 3, 4, 5, 6, 7, 8 ).exists( _ == 6 ) must beTrue
    }

    "does not find a non-existing item" in {
      Stream(1, 2, 3, 4, 5).exists( _ == 6) must beFalse
    }

    "for all returns true if all items match" in {
      Stream(1, 2, 3, 4, 5).forAll( _ < 6) must beTrue
    }

    "for all returns false any of the items do not match" in {
      Stream(1, 2, 3, 4, 5).forAll( _ != 3) must beFalse
    }

    "map over numbers" in {
      Stream(1, 2, 3, 4, 5).map( _ * 2).toList === List(10, 8, 6, 4, 2)
    }

    "filter even numbers" in {
      Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList === List(6, 4, 2)
    }

    "appends two streams" in {
      Stream(1, 2, 3).append(Stream(4, 5, 6)).toList === List(6, 5, 4, 3, 2, 1)
    }

    "flat maps the stream" in {
      Stream(1, 3 ,5).flatMap {
        value =>
          Stream(value, value + 1)
      }.toList === List(6, 5, 4, 3, 2, 1)
    }

    "produces a sequence of the constant value" in {
      Stream.constant(1).take(5).toList === List(1,1,1,1,1)
    }

    "produces a sequence of ever increasing numbers" in {
      Stream.from(10).take(4).toList === List(13, 12, 11, 10)
    }

    "produces the fibbonaci sequence" in {
      Stream.fibs().take(6).toList === List(8, 5, 3, 2, 1, 1)
    }

    "checks if a stream starts with some values" in {
      Stream(1, 2, 3, 4).startsWith(Stream(1, 2)) must beTrue
    }

    "checks if a stream does not start with some values" in {
      Stream(1, 2, 3, 4).startsWith(Stream(2, 3)) must beFalse
    }

    "works even when the prefix is bigger than the original" in {
      Stream(1, 2).startsWith(Stream(1, 2, 3)) must beFalse
    }

  }

}
