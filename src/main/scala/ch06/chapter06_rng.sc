import ch06._

// ex 1
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, newRng) = rng.nextInt
  if (n >= 0) (n, newRng)
  else if (n == Int.MinValue) (Int.MaxValue, newRng)
  else (-n, newRng)
}

assert(nonNegativeInt(Simple(1))._1 >= 0)
