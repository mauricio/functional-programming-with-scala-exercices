package chapter2

/**
 * User: Maurício Linhares
 * Date: 8/3/12
 * Time: 4:17 PM
 */



object HigherOrder {

	type Pred[A] = A => Boolean

	def isEven(n: Int): Boolean = isDivisableBy( 2 )( n )

	def isDivisableBy( n : Int ) : Pred[Int] = {
		other =>  other % n == 0
	}

	def isDivisableByThree  = isDivisableBy(3)
	def isDivisableByFive = isDivisableBy(5)

	def isDivisableByThreeAndFive : Pred[Int] = {
    lift( isDivisableBy(3), isDivisableBy(5), (n : Int, left : Pred[Int], right : Pred[Int]) => { left(n) && right(n) }  )
	}

  def isDivisableByThreeOrFive : Pred[Int] = {
    lift( isDivisableBy(3), isDivisableBy(5), (n : Int, left : Pred[Int], right : Pred[Int]) => { left(n) || right(n) }  )
  }

  def lift[I,O]( left : I => O,  right : I => O,  condition : ( I, I => O,  I => O ) => O ) : I => O = {
    ( element : I ) => {
      condition( element, left, right )
    }
  }

  def applyIf[A]( a : A, f : A => A, p : A => Boolean ) : A = {
    if ( p(a) ) f(a) else a
  }

}
