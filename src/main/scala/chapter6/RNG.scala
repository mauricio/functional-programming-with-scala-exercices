package chapter6

import scala.annotation.tailrec

trait RNG {

  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int,RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG)

  def double(rng: RNG): (Double, RNG) =
    map(r => r.nextInt)(i => i.toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG)

  def doubleInt(rng: RNG): ((Double, Int), RNG)

  def double3(rng: RNG): ((Double, Double, Double), RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val int: Rand[Int] = _.nextInt
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val ar = ra(rng)
    val br = rb(ar._2)
    f(ar._1, br._1) -> br._2
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldLeft(List[A]() -> rng)((acc,item) => {
      val (result,next) = item(acc._2)
      (result :: acc._1) -> next
    })
  }

}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  @tailrec final def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val pair = rng.nextInt
    if (pair._1 == Int.MinValue) {
      nonNegativeInt(pair._2)
    } else {
      (Math.abs(pair._1), pair._2)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    rng.nextInt match {
      case (int, next) => double(next) match {
        case (double, other) => ((int, double), other)
      }
    }
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val pair = intDouble(rng)
    ((pair._1._2, pair._1._1), pair._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val first = double(rng)
    val second = double(first._2)
    val third = double(second._2)
    ((first._1, second._1, third._1), third._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    1.to(count).foldLeft(List[Int]() -> rng)((acc, item) => {
      val value = acc._2.nextInt
      (value._1 :: acc._1) -> value._2
    })
  }

}


