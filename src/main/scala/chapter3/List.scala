package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, end: List[A]) extends List[A]

object List {

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(xs, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def setHead[A]( l : List[A], item : A ) : List[A] = {
    l match {
      case Nil => Cons(item, Nil)
      case Cons(x, xs) => Cons(item, xs)
    }
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def drop[A]( list : List[A], n : Int ) : List[A] = {
    n match {
      case 0 => list
      case _ => {
        list match {
          case Cons(x, xs) => drop(xs, n - 1)
          case Nil => throw new UnsupportedOperationException("You can't drop from a Nil list")
        }
      }
    }
  }

  def tail[A]( list : List[A] ) : List[A] = drop(list, 1)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
     l match {
       case Nil => Nil
       case Cons(x, xs) => {
         if (f(x)) {
           dropWhile(xs)(f)
         } else {
           l
         }
       }
     }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

}
