package chapter2

import scala.annotation.tailrec

object Chapter2 {

  def fib(n: Int): Int = {
    n match {
      case 0 | 1 => n
      case _ => {
        @tailrec
        def go(current: Int, next: Int, acc: Int): Int = {
          if (acc == 0) {
            current + next
          } else {
            go(next, current + next, acc - 1)
          }
        }
        go(0, 1, n - 2)
      }
    }

  }

  def isSorted[@specialized A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @tailrec
    def go( items : Array[A], start : Int ) : Boolean = {
      if ( start >= (items.length - 1) ) {
        true
      } else {
        if ( gt(items(start), items(start + 1)) ) {
          false
        } else {
          go(items, start + 1)
        }
      }
    }
    go(as, 0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = ( v : B ) => f(a,v)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a : A) => ( b : B ) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a : A, b : B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a : A) => f(g(a))

}