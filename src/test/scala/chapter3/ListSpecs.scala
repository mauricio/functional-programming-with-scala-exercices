package chapter3

import org.specs2.mutable.Specification

/**
 * User: mauricio
 * Date: 5/19/13
 * Time: 2:32 PM
 */
class ListSpecs extends Specification {

  val items = List(1, 2, 3, 4)

  "list" should {

    "drop the tail correctly" in {

      List.tail(items) === List(2, 3, 4)
    }

    "drop 2 items" in {
      List.drop(items, 2) === List( 3, 4)
    }

    "drop while item is less than 3" in {
      List.dropWhile(items)( i => i <= 3 ) === List( 4)
    }

    "set head" in {
      List.setHead(items, 10) === List(10, 2, 3, 4)
    }

    "init removing last item" in {
      List.init(items) === List(1, 2, 3)
    }

  }

}
