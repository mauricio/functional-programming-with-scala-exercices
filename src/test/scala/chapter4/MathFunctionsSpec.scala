package chapter4

import org.specs2.mutable.Specification

/**
 * User: mauricio
 * Date: 9/24/13
 * Time: 11:00 PM
 *
fun countup_from1 (x:int) =
let fun count (from:int, to:int) =
if from=to
then to::[]
else from :: count(from+1,to)
in
count(1,x)
end

 *
 */

object Counter {

  def countUpToOne( x : Int ) : List[Int] = {
    def count( from : Int, to : Int ) : List[Int] = {
      if (from == to) List(to) else from :: count(from - 1, to)
    }
    count(1,x)
  }

}

class MathFunctionsSpec extends Specification {

  "math functions" should {

    "correctly create a sequence" in {

      val parsedArray = Array( Array( "one", "two", "three" ), Array( "four", "five", "six", "seven" ) )

      val fun = (item : String) => item

      val result : Array[Array[String]] = parsedArray.map(
        line =>
          line.zipWithIndex.map {
            word =>
              word._2 match {
                case 2 | 3 => fun(word._1)
                case _ => word._1
              }
          }
      )

      true === true

    }

  }

}
