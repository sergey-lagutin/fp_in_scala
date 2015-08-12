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

