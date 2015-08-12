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

