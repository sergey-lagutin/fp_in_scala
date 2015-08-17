package ch06

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a1, s1) = run(s)
    (f(a1), s1)
  })

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (a1, s1) = run(s)
    g(a1).run(s1)
  })
}


object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit(List[A]()): State[S, List[A]])((s, acc) => s.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}