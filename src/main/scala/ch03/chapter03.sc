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


// ex 16
def addOne(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case Cons(h, t) => Cons(h + 1, addOne(t))
}

assert(addOne(Nil) == Nil)
assert(addOne(List(1, 2, 3)) == List(2, 3, 4))


// ex 17
def toString(l: List[Double]): List[String] = l match {
  case Nil => Nil
  case Cons(h, t) => Cons(h.toString, toString(t))
}

assert(toString(Nil) == Nil)
assert(toString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))


// ex 18
def map[A, B](l: List[A])(f: A => B): List[B] = l match {
  case Nil => Nil
  case Cons(h, t) => Cons(f(h), map(t)(f))
}

assert(map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
assert(map(Nil: List[Int])(_ + 1) == Nil)


// 19
def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(h, t) =>
    if (f(h)) Cons(h, filter(t)(f))
    else filter(t)(f)
}

assert(filter(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))


// ex 20
def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
  flatten(map(l)(f))

assert(flatMap(List(1, 2, 3))(x => List(x * 2)) == List(2, 4, 6))


// ex 21
def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
  flatMap(l)(a => if (f(a)) List(a) else Nil)

assert(filterUsingFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))


// ex 22
def addElements(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
  case (Nil, Nil) => Nil
  case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addElements(xs, ys))
}

assert(addElements(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))


// ex 23
def combine[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
  case (Nil, Nil) => Nil
  case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), combine(xs, ys)(f))
}

assert(combine(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))


// ex 24
def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
  def isStartWith[A](a: List[A], b: List[A]): Boolean =
    (a, b) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) =>
        if (x == y) isStartWith(xs, ys)
        else false
    }

  (l, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xs), Cons(y, ys)) =>
      if (x == y) isStartWith(xs, ys)
      else hasSubsequence(xs, sub)
  }
}

assert(!hasSubsequence(Nil, List(1)))
assert(hasSubsequence(List(1), Nil))
assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
assert(!hasSubsequence(List(1, 2, 3, 4), List(1, 3)))
