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
