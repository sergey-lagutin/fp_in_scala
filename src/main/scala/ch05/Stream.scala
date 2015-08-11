package ch05

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Stream.empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty[A])

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Option(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[AA >: A](aa: => Stream[AA]): Stream[AA] =
    foldRight(aa)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapUsingFold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Option((f(h()), t()))
      case Empty => None
    }

  def takeUsingFold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (stream, value) => stream match {
        case Cons(h, t) if value > 0 => Option((h(), (t(), value - 1)))
        case _ => None
      }
    }

  def takeWhileUsingFold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Option((h(), t()))
      case _ => None
    }

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, that)) {
      case ((s1, s2)) => (s1, s2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Option((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, that)) {
      case (s1, s2) => (s1, s2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Option((Option(h1()), Option(h2())), (t1(), t2()))
        case (Empty, Cons(h2, t2)) => Option((None, Option(h2())), (Empty, t2()))
        case (Cons(h1, t1), Empty) => Option((Option(h1()), None), (t1(), Empty))
        case (Empty, Empty) => None
      }
    }

  def startsWith[B](that: Stream[B]): Boolean =
    zipAll(that).takeWhile(_._2.isDefined).forAll {
      case (a, b) => a == b
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case stream@Cons(h, t) => Option((stream, t()))
      case Empty => None
    } append Stream(Stream.empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.map(_.foldRight(z)(f))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(x0: Int, x1: Int): Stream[Int] =
      cons(x0, go(x1, x0 + x1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
}