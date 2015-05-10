package chapter3

import scala.annotation.tailrec

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

  def setHead[A](l: List[A], item: A): List[A] = {
    l match {
      case Nil => Cons(item, Nil)
      case Cons(x, xs) => Cons(item, xs)
    }
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def drop[A](list: List[A], n: Int): List[A] = {
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

  def tail[A](list: List[A]): List[A] = drop(list, 1)

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
    foldRight(a1, a2){ (value, acc) => Cons(value, acc) }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) = foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldRight(l, 0) {
    (input, output) => output + 1
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0) {
    (acc, value) => acc + value
  }

  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0) {
    (acc, value) => acc * value
  }

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0) {
    (acc, value) => acc + 1
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) {
    (acc, value) => Cons( value, acc)
  }

  def flatten[A](l : List[List[A]]) : List[A] = l match {
    case Nil => Nil
    case Cons( x, xs ) => append(x, flatten(xs))
  }

  def plusOne( l : List[Int] ) : List[Int] = foldRight(l, Nil : List[Int]){ (value, acc) => Cons(value + 1, acc) }

  def toString( l : List[Double] ) : List[String] = foldRight(l, Nil : List[String]){ (value, acc) => Cons(value.toString, acc) }

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil : List[B]){ (value, acc) => Cons(f(value), acc) }

  def filter[A]( l : List[A] )( f : A => Boolean ) : List[A] = {
    foldRight(l, Nil : List[A]) { (value, acc) =>
      if ( f(value) ) {
        Cons(value, acc)
      } else {
        acc
      }
    }
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight(l, Nil : List[B]) { (value, acc) =>
      append(f(value), acc)
    }
  }

  def zipPlus( l : List[Int], r : List[Int] ) : List[Int] = {

      l match {
        case Nil => Nil
        case Cons(x, xs) => r match {
          case Nil => Nil
          case Cons(y, ys) => Cons(x + y, zipPlus(xs, ys))
        }
      }

  }

  def zip[A,B]( l : List[A], r : List[A] )(f : (A,A) => B) : List[B] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => r match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(x, y), zip(xs, ys)(f))
      }
    }
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    l match {
      case Nil => l == sub
      case Cons(x, xs) => sub match {
        case Nil => true
        case Cons(y, ys) => if ( x == y ) {
          startsWith(xs, ys)
        } else {
          hasSubsequence(xs, sub)
        }
      }
    }
  }

  def startsWith[A]( l : List[A], sub : List[A] ) : Boolean = {
    l match {
      case Nil => l == sub
      case Cons(x, xs) => sub match {
        case Nil => true
        case Cons(y, ys) => if ( x == y ) {
          startsWith(xs, ys)
        } else {
          false
        }
      }
    }
  }

}