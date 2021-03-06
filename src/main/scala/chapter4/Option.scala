package chapter4

trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  def map[B](f: (A) => B): Option[B] = Some(f(get))

  def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)

  def getOrElse[B >: A](default: => B): B = get

  def orElse[B >: A](ob: => Option[B]): Option[B] = this

  def filter(f: (A) => Boolean): Option[A] = if( f(get) ) this else None
}

case object None extends Option[Nothing] {
  def map[B](f: (Nothing) => B): Option[B] = this

  def flatMap[B](f: (Nothing) => Option[B]): Option[B] = this

  def getOrElse[B >: Nothing](default: => B): B = default

  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  def filter(f: (Nothing) => Boolean): Option[Nothing] = this
}