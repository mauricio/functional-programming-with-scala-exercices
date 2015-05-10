package chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def isEmpty : Boolean = this match {
    case Empty => true
    case _ => false
  }

  def headOption: Option[A] =
    foldRight(Option.empty[A])((h,t) => Some(h))

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = {
    this match {
      case Empty => acc
      case Cons(hd, tl) => f(hd(), tl().foldRight(acc)(f))
    }
  }

  def exists(f: A => Boolean): Boolean =
    foldRight(false)((h, t) => f(h) || t)

  def forAll( f: A => Boolean ) : Boolean =
    foldRight(true)((h, t) => f(h) && t)

  @tailrec
  final def foldLeft[B](init: B)(f: (A, B) => B): B = this match {
    case Empty => init
    case Cons(head, tail) => tail().foldLeft(f(head(), init))(f)
  }

  def toList: List[A] = foldLeft(List[A]())((head, acc) => head :: acc)

  def takeWhile(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t )

  def take(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(hd, tl) => if (n == 0) empty else cons(hd(), tl().take(n - 1))
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(hd, tl) => if (n == 0) this else tl().drop(n - 1)
  }

  def map[B](f : A => B) : Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f : A => Boolean) : Stream[A] =
    foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t )

  def append[S >: A](s : Stream[S]) : Stream[S] =
    foldRight(s)((h,t) => cons(h, t))

  def flatMap[B](f : A => Stream[B]) : Stream[B] =
    foldRight(empty[B])((h,t) => f(h).append(t))

  def startsWith[A](s: Stream[A]): Boolean = {
    s match {
      case Empty => true
      case Cons(h, t) => this match {
        case Empty => false
        case Cons(th, tt) => h() == th() && tt().startsWith(t())
      }
    }
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a : A) : Stream[A] =
    unfold(a)((n) => Some(n -> n))

  def from(a : Int) : Stream[Int] =
    unfold(a)((n) => Some(n -> (n + 1)))

  def fibs() : Stream[Int] =
    cons(1, unfold(0 -> 1)((pair) => Some((pair._1 + pair._2) -> ( pair._2 -> (pair._1 + pair._2) ))))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((current, next)) => cons(current, unfold(next)(f))
      case None => empty[A]
    }
  }

}