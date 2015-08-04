import ch04._

// ex 2
def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs).flatMap(m =>
    mean(xs.map(x => math.pow(x - m, 2)))
  )
}

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

assert(mean(List()) == None)
assert(mean(List(1, 2, 3)) == Some(2))


// ex 3
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(as => b.map(bs => f(as, bs)))

assert(map2(None: Option[Int], Some(2))(_ + _) == None)
assert(map2(Some(2), None: Option[Int])(_ + _) == None)
assert(map2(Some(2), Some(5))(_ + _) == Some(7))


// ex 4
def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a.foldLeft(Option(List[A]())) {
    (list: Option[List[A]], opt: Option[A]) => opt.flatMap(a => list.map(_ :+ a))
  }
}

assert(sequence(List(Some(1), None)) == None)
assert(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))


