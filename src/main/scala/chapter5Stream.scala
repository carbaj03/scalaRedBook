import chapter6.Stream.{cons, empty}

object chapter5Stream {

  sealed trait Stream[+A] {
    def toListRecursive: List[A] = this match {
      case Cons(h, t) => h() :: t().toListRecursive
      case _ => List()
    }

    def toList: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List()).reverse
    }

    def toListFast: List[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      @annotation.tailrec
      def go(s: Stream[A]): List[A] = s match {
        case Cons(h, t) =>
          buf += h()
          go(t())
        case _ => buf.toList
      }

      go(this)
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
      case _ => empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def existsR(f: A => Boolean): Boolean =
      foldRight(false)((h, t) => f(h) || t)

    def forAll(f: A => Boolean): Boolean =
      foldRight(true)((h, t) => f(h) && t)

    def takeWhileR(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else empty)

    def headOption: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](q: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(q(h), t))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Empty[B])((h, t) => f(h) append t)

    def find(p: A => Boolean): Option[A] =
      filter(p).headOption

    val ones: Stream[Int] =
      cons(1, ones)

    def constant[A](a: A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail
    }

    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

    def fibs(): Stream[Int] = {
      def go(f0: Int, f1: Int): Stream[Int] =
        cons(f0, go(f1, f0 + f1))

      go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((h, s)) => cons(h, unfold(s)(f))
        case None => Empty
      }

    val onesViaUnfold =
      unfold(1)(_ => Some((1, 1)))

    val fibsViaUnfold =
      unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

    def constantViaUnfold[A](a: A) =
      unfold(a)(_ => Some((a, a)))

    def fromViaUnfold(n: Int) =
      unfold(n)(n => Some((n, n + 1)))

    def mapViaUnfold[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }

    def takeViaUnfold(n: Int): Stream[A] =
      unfold((this, n)) {
        case (Cons(h, t), 1) => Some((h(), (empty, 0)))
        case (Cons(h, t), m) if m > 1 => Some((h(), (t(), m - 1)))
        case _ => None
      }

    def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if f(h()) => Some((h(), t()))
        case _ => None
      }

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }

    // special case of `zipWith`
    def zip[B](s2: Stream[B]): Stream[(A, B)] =
      zipWith(s2)((_, _))

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWith(s2)()

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h,t), Empty) =>  Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      }
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
  }

  def main(args: Array[String]): Unit = {

    Stream.apply("a").toListRecursive
  }
}


