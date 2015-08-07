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
