import ch04.{Left, Right, Either}

// ex 7
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  es.foldRight(Right(List[A]()): Either[E, List[A]]) {
    (e, list) => e.map2(list)(_ :: _)
  }

assert(sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
assert(sequence(List(Right(1), Left(3))) == Left(3))
assert(sequence(List(Left(1), Left(3))) == Left(1))


def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  as.foldRight(Right(List[B]()): Either[E, List[B]]) {
    (e, list) => f(e).map2(list)(_ :: _)
  }

def sqrt(i: Int): Either[String, Double] =
  if (i >= 0) Right(math.sqrt(i))
  else Left("Should be positive")

assert(traverse(List(1, 4))(sqrt) == Right(List(1, 2)))
assert(traverse(List(1, -1))(sqrt) == Left("Should be positive"))

