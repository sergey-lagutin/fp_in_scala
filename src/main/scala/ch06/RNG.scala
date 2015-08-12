package ch06

trait RNG {
  def nextInt: (Int, RNG)
}
