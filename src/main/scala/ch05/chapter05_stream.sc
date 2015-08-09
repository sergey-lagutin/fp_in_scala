import ch05._

// ex 1
assert(Stream.empty.toList == Nil)
assert(Stream(1, 2, 3).toList == List(1, 2, 3))


// ex 2
assert(Stream.empty.take(2).toList == Nil)
assert(Stream(1, 2, 3).take(2).toList == List(1, 2))

assert(Stream.empty.drop(2).toList == Nil)
assert(Stream(1, 2, 3).drop(1).toList == List(2, 3))

// ex 3
assert(Stream.empty[Int].takeWhile(x => true).toList == Nil)
assert(Stream(1, 2, -1, 1, 2).takeWhile(_ > 0).toList == List(1, 2))


// ex 4
assert(Stream(1, 2, 3).forAll(_ > 0))
assert(!Stream(1, -2, 3).forAll(_ > 0))


// ex 5
assert(Stream.empty[Int].takeWhile2(x => true).toList == Nil)
assert(Stream(1, 2, -1, 1, 2).takeWhile2(_ > 0).toList == List(1, 2))


// ex 6
assert(Stream.empty.headOption == None)
assert(Stream(1, 2, 3).headOption == Some(1))


// ex 7
assert(Stream.empty[Int].map(_ * 2).toList == Nil)
assert(Stream(1, 2, 3).map(_ * 2).toList == List(2, 4, 6))

assert(Stream.empty[Int].filter(_ != 0).toList == Nil)
assert(Stream(1, 0, 2, 0, 3).filter(_ != 0).toList == List(1, 2, 3))

assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))

def list(n: Int): Stream[Int] = Stream((1 to n).toList: _*)
assert(Stream.empty[Int].flatMap(list).toList == Nil)
assert(Stream(1, 2, 3).flatMap(list).toList == List(1, 1, 2, 1, 2, 3))


// ex 8
assert(Stream.constant(5).take(2).toList == List(5, 5))


// ex 9
assert(Stream.from(2).take(5).toList == List(2, 3, 4, 5, 6))


// ex 10
assert(Stream.fibs.take(3).toList == List(0, 1, 1))
assert(Stream.fibs.take(6).toList == List(0, 1, 1, 2, 3, 5))


// ex 11
val fibs2: Stream[Int] = Stream.unfold((0, 1)) {
  case (x0, x1) => Some((x0, (x1, x0 + x1)))
}

assert(fibs2.take(3).toList == List(0, 1, 1))
assert(fibs2.take(6).toList == List(0, 1, 1, 2, 3, 5))


// ex 12
def from2(n: Int): Stream[Int] =
  Stream.unfold(n)(x => Option((x, x + 1)))

assert(from2(2).take(5).toList == List(2, 3, 4, 5, 6))

def constant2[A](n: A): Stream[A] =
  Stream.unfold(n)(_ => Option(n, n))

assert(Stream.constant(5).take(2).toList == List(5, 5))

val ones: Stream[Int] = Stream.unfold(1)(_ => Option(1, 1))
assert(ones.take(2).toList == List(1, 1))
