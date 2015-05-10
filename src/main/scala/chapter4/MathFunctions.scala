package chapter4

import java.util.regex.{Pattern, PatternSyntaxException}

object MathFunctions {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs : Seq[Double]) : Option[Double] = {
    mean(xs).flatMap { value =>
      mean(xs.map( x => math.pow(x - value, 2) ))
    }
  }

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aValue <- a
      bValue <- b
    } yield f(aValue, bValue)
  }

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2)){ (a,b) => a(s) && b(s) }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

  }

}
