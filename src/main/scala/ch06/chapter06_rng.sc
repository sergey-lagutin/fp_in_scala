import ch06.RNG._
import ch06._

// ex 1
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, newRng) = rng.nextInt
  if (n >= 0) (n, newRng)
  else if (n == Int.MinValue) (Int.MaxValue, newRng)
  else (-n, newRng)
}

assert(nonNegativeInt(Simple(1))._1 >= 0)


// ex 2
def double(rng: RNG): (Double, RNG) = {
  val (n, next) = nonNegativeInt(rng)
  (n / (Int.MaxValue.toDouble + 1), next)
}

assert(double(Simple(10))._1 >= 0)
assert(double(Simple(10))._1 < 1)

// ex 3
def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (i, iRng) = rng.nextInt
  val (d, dRng) = double(iRng)
  ((i, d), dRng)
}


def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val ((i, d), newRng) = intDouble(rng)
  ((d, i), newRng)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (d1, rng1) = double(rng)
  val (d2, rng2) = double(rng1)
  val (d3, rng3) = double(rng2)
  ((d1, d2, d3), rng3)
}


// ex 4
def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  if (count <= 0) (List(), rng)
  else {
    val (i, r) = rng.nextInt
    val (list, r2) = ints(count - 1)(r)
    (i :: list, r2)
  }
}

assert(ints(5)(Simple(123))._1.size == 5)


def nonNegativeEven: Rand[Int] =
  map(nonNegativeInt)(i => i - i % 2)


// ex 5
def _double: Rand[Double] =
  map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

assert(_double(Simple(10))._1 >= 0)
assert(_double(Simple(10))._1 < 1)


// ex 6
def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (v1, r1) = ra(rng)
    val (v2, r2) = rb(r1)
    (f(v1, v2), r2)
  }

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
  map2(ra, rb)((_, _))

val randIntDouble: Rand[(Int, Double)] =
  both(int, double)

val randDoubleInt: Rand[(Double, Int)] =
  both(double, int)


// ex 7
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  rng =>
    fs.foldRight((List[A](), rng)) {
      (r0, acc) =>
        val (v, r1) = r0(acc._2)
        (v :: acc._1, r1)
    }

assert(sequence(List(unit(1), unit(2), unit(3)))(Simple(1))._1 == List(1, 2, 3))

def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

def _ints(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(int))


// ex 8
def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (v1, r1) = f(rng)
    g(v1)(r1)
  }

def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      unit(mod)
    else nonNegativeLessThan(n)
  }


// ex 9
def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))

def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(ra)(a => map(rb)(b => f(a, b)))
