package chapter6

import scala.annotation.tailrec

trait RNG {

  def nextInt: (Int, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG)

  def double(rng : RNG) : (Double,RNG) =
    State.map(State[RNG,Int](r => r.nextInt))(i =>  i.toDouble / Int.MaxValue ).run(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG)

  def doubleInt(rng: RNG): ((Double, Int), RNG)

  def double3(rng: RNG): ((Double, Double, Double), RNG)

  def both[A, B](ra: State[RNG,A], rb: State[RNG,B]): State[RNG,(A, B)] = State.map2(ra, rb)((_, _))

  val int: State[RNG,Int] = State(_.nextInt)
  val randIntDouble: State[RNG,(Int, Double)] = both(int, State(double))
  val randDoubleInt: State[RNG,(Double, Int)] = both(State(double), int)

  def nonNegativeEven: State[RNG,Int] = State.map(State(nonNegativeInt))(i => i - i % 2)

  def rollDie: State[RNG,Int] = nonNegativeLessThan(6)

  def nonNegativeLessThan(n: Int): State[RNG,Int] =
    State.flatMap(State(nonNegativeInt))({
      i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) {
          State(rng => { (n, rng) })
        }
        else {
          nonNegativeLessThan(n)
        }
    })

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


