import ch03.List
import ch03.Cons
import ch03.Nil

import scala.annotation.tailrec


// ex 2
def tail[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(h, t) => t
}

assert(tail(Nil) == Nil)
assert(tail(List(1, 2, 3)) == List(2, 3))


// ex 3
def setHead[A](l: List[A], e: A) = l match {
  case Nil => Nil
  case Cons(h, t) => Cons(e, t)
}

assert(setHead(Nil, 0) == Nil)
assert(setHead(List(1, 2, 3), 4) == List(4, 2, 3))


// ex 4
@tailrec
def drop[A](l: List[A], n: Int): List[A] =
  if (n == 0) l
  else l match {
    case Nil => Nil
    case Cons(x, xs) => drop(xs, n - 1)
  }

assert(drop(Nil, 5) == Nil)
assert(drop(List(1, 2, 3), 5) == Nil)
assert(drop(List(1, 2, 3), 2) == List(3))


// ex 5
@tailrec
def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(x, xs) =>
    if (p(x)) dropWhile(xs, p)
    else l
}

assert(dropWhile(List(1, 2), (x: Int) => x < 0) == List(1, 2))
assert(dropWhile(List(-2, -1, 1, 2, -2, -1), (x: Int) => x < 0) == List(1, 2, -2, -1))


// ex 6
def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(x, Nil) => Nil
  case Cons(x, xs) => Cons(x, init(xs))
}

assert(init(Nil) == Nil)
assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))


def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }


// ex 9
def length[A](l: List[A]): Int =
  foldRight(l, 0)((_, b) => b + 1)

assert(length(Nil) == 0)
assert(length(List(1, 2, 3)) == 3)


// ex 10
@tailrec
def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

assert(foldLeft(Nil: List[Int], 0)(_ + _) == 0)
assert(foldLeft(List(1, 2, 3), 0)(_ + _) == 6)


// ex 11
def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

assert(sum(Nil) == 0)
assert(sum(List(1, 2)) == 3)

def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

assert(product(Nil) == 1)
assert(product(List(1, 2, 3)) == 6)

def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, _) => b + 1)

assert(lengthLeft(Nil) == 0)
assert(lengthLeft(List(1, 2, 3)) == 3)


// ex 12
def reverse[A](l: List[A]): List[A] =
  foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

assert(reverse(Nil) == Nil)
assert(reverse(List(1, 2, 3)) == List(3, 2, 1))


// ex 13
def foldLeftUsingRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  foldRight(l, z)((a, b) => f(b, a))

assert(foldLeftUsingRight(List(1, 2, 3), 0)(_ + _) == 6)

def foldRightUsingLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
  foldLeft(l, z)((b, a) => f(a, b))

assert(foldRightUsingLeft(List(1, 2, 3), 0)(_ + _) == 6)


// ex 14
def append[A](l: List[A], a: A): List[A] =
  foldRight(l, List(a))(Cons(_, _))

def appendAll[A](a: List[A], b: List[A]): List[A] =
  foldRight(a, b)(Cons(_, _))

assert(append(Nil, 1) == List(1))
assert(append(List(1, 2, 3), 1) == List(1, 2, 3, 1))


// ex 15
def flatten[A](as: List[List[A]]): List[A] =
  foldRight(as, Nil: List[A])(appendAll)

assert(flatten(List(List(1), List(2), List(3))) == List(1, 2, 3))
