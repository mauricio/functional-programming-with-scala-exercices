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
    unfold(this)((s) => s match {
        case Empty => None
        case Cons(hd, tl) => if (!f(hd())) None else Some(hd() -> tl())
      }
    )

  def take(n: Int): Stream[A] =
    unfold(this -> n)((pair) => pair match {
      case (s,position) => s match {
        case Empty => None
        case Cons(hd, tl) => if (position == 0) None else Some(hd() -> ( tl() -> (position -1) ))
      }
    })

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(hd, tl) => if (n == 0) this else tl().drop(n - 1)
  }

  def map[B](f : A => B) : Stream[B] =
    unfold(this)((s) => s match {
      case Empty => None
      case Cons(hd, tl) => Some(f(hd()) -> tl())
    })

  def filter(f : A => Boolean) : Stream[A] =
    foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t )

  def append[S >: A](s : Stream[S]) : Stream[S] =
    foldRight(s)((h,t) => cons(h, t))

  def flatMap[B](f : A => Stream[B]) : Stream[B] =
    foldRight(empty[B])((h,t) => f(h).append(t))

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll((pair) => pair._1 == pair._2)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this -> s2)((pair) => pair match {
      case (Empty, Empty) => None
      case (Empty, Cons(rh,rt)) => Some((None -> Some(rh())) -> ( Empty -> rt() ))
      case (Cons(lh,lt), Empty) => Some((Some(lh()) -> None) -> ( lt() -> Empty ))
      case (Cons(lh, lt), Cons(rh, rt)) => Some((Some(lh()) -> Some(rh())) -> ( lt() -> rt() ))
    })

  def tails : Stream[Stream[A]] =
    scanRight(empty[A])((current,acc) => cons(current,acc))

  def scanRight[B](end : B)(f : (A,B) => B) : Stream[B] = {
    unfold(this)((s) => s match {
      case Empty => None
      case Cons(_, tl) => {
        val result = s.foldRight(end)((h,t) => f(h, t))
        Some(result -> tl())
      }
    })
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  override def equals(other : Any) : Boolean = {
    other match {
      case o: Stream[A] => o.zipAll(this).forAll((pair) => pair._1 == pair._2)
      case _ => false
    }
  }

}

case object Empty extends Stream[Nothing] {

  override def equals(other : Any) : Boolean = other match {
    case o : AnyRef => this.eq(o)
    case _ => false
  }

}

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