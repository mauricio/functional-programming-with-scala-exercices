package chapter2

/**
 * User: Maurício Linhares
 * Date: 8/3/12
 * Time: 3:08 PM
 */

object Absolute {

	val abs : Int => Int = ( number ) => if (number < 0) - number else number

	def main( args : Array[String] ) {
		assert( abs( -42 ) == 42 )
	}

}