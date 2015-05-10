package chapter3

import org.specs2.mutable.Specification
import java.nio.charset.Charset

class ListSpecs extends Specification {

  final val items = List(1, 2, 3, 4)
  final val doubles = List[Double](1, 2, 3, 4)

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

    "length of a list" in {
      List.length(items) === 4
    }

    "left sum" in {
      List.sumLeft(items) === 10
    }

    "left length" in {
      List.lengthLeft(items) === 4
    }

    "left product" in {
      List.productLeft(doubles) === 24
    }

    "reverse a list" in {
      List.reverse(items) === List(4, 3, 2, 1)
    }

    "append to a list" in {
      List.append(items, List(5, 6, 7, 8)) === List(1, 2, 3, 4, 5, 6, 7, 8)
    }

    "flatten a list of lists" in {
      List.flatten(List(items, List(5, 6, 7, 8), List(9, 10, 11, 12))) === List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    }

    "plus one a list" in {
      List.plusOne(items) === List(2, 3, 4, 5)
    }

    "toString a list" in {
      List.toString(List(1, 2, 3, 4)) === List("1.0", "2.0", "3.0", "4.0")
    }

    "filter a list" in {
      List.filter(items)( v => (v % 2) == 0 ) === List(2, 4)
    }

    "flat map" in {
      List.flatMap(List(1,2,3))(i => List(i,i)) === List(1,1,2,2,3,3)
    }

    "zipping plus" in {
      List.zipPlus( List(1,2,3), List(4,5,6) ) === List(5,7,9)
    }

    "has subsequence" in {
      List.hasSubsequence(List(1,2,3,4), List(2,3)) must beTrue
      List.hasSubsequence(List(1,2,3,4), List(3,2)) must beFalse
      List.hasSubsequence(List(1,2,3,4), List(4))   must beTrue
      List.hasSubsequence(List(1,2,3,4), List(3,4)) must beTrue
      List.hasSubsequence(List(1,2,3,4), List(1,2)) must beTrue
    }

  }

}
