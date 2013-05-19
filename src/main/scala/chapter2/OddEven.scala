package chapter2

/**
 * User: Maurício Linhares
 * Date: 8/3/12
 * Time: 3:13 PM
 */

object OddEven {

	def isEven(n: Int): Boolean = n % 2 == 0

	def not( f : Int => Boolean ) : Int => Boolean = {
		( n ) => !f(n)
	}

  def isOdd( n : Int ) : Boolean = not(isEven)(n)

}
