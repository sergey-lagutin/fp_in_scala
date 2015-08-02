import scala.annotation.tailrec

// ex 1
def fib(n: Int): Int = {
  def go(n: Int, a: Int, b: Int): Int =
    if (n == 0) a + b
    else go(n - 1, b, a + b)

  assert(n >= 0)
  if (n < 2) n
  else go(n - 2, 0, 1)
}
assert(fib(0) == 0)
assert(fib(1) == 1)
assert(fib(6) == 8)


// ex 2
@tailrec
def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
  if (as.length < 2) true
  else if (gt(as(1), as(0))) isSorted(as.tail, gt)
  else false

assert(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a > b))
assert(!isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b))


// ex 3
def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

assert(curry((a: Int, b: Int) => a + b)(1)(2) == 3)


// ex 4
def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

assert(uncurry(curry((a: Int, b: Int) => a + b))(1, 2) == 3)


// ex 5
def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

assert(compose((x: Int) => x * 2, (x: Int) => x - 1)(5) == 8)


