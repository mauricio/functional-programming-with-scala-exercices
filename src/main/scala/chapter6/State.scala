package chapter6

object State {

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = State(rng => {
    fs.foldLeft(List[A]() -> rng)((acc, item) => {
      val (result, next) = item.run(acc._2)
      (result :: acc._1) -> next
    })
  })

  def flatMap[S,A,B](f: State[S,A])(g: A => State[S,B]): State[S,B] = State(rng => {
    val (a,next) = f.run(rng)
    g(a).run(next)
  })

  def map[S, A, B](s: State[S,A])(f: A => B): State[S,B] =
    flatMap(s)(a => State(rng => (f(a), rng)))

  def map2[S, A, B, C](ra: State[S,A], rb: State[S,B])(f: (A, B) => C): State[S,C] =
    flatMap(ra)(aValue => map(rb)(bValue => f(aValue, bValue)))

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

}

final case class State[S,+A](run: S => (A,S)) {


}