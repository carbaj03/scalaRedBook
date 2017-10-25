

object d {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum(ns: List[Int]): Int =
      foldRight(ns, 0)((x, y) => x + y)

    def product(ns: List[Double]): Double =
      foldRight(ns, 1.0)(_ * _)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, acc) => acc + 1)

    /*
f(x, foldRight(xs, z)(f))
f(h , foldRight(tail , 0) (_, 0) => 0 + 1)
f(h , foldRight(tail , 1) (_, 1) => 1 + 1)
f(h , foldRight(tail , 2) (_, 2) => 2 + 1)
 */
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def sumL(ns: List[Int]): Int =
      foldLeft(ns, 0)((x, y) => x + y)

    def productL(ns: List[Double]): Double =
      foldLeft(ns, 1.0)(_ * _)

    def lengthL[A](l: List[A]): Int =
      foldLeft(l, 0)((acc, _) => acc + 1)

    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

    def foldRightL[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(l), z)((b, a) => f(a, b))

    def foldRightL2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    def appendR[A](l: List[A], r: List[A]): List[A] =
      foldRight(l, r)((x, y) => Cons(x, y))

    def concat[A](l: List[List[A]]): List[A] =
      foldLeft(l, List[A]())(appendR)

    def mapInt(l: List[Int]): List[Int] =
      foldRightL(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    def mapDoubleString(l: List[Double]): List[String] =
      foldRightL(l, Nil: List[String])((h, t) => Cons(h.toString, t))


    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

    def mapL[A, B](l: List[A])(f: A => B): List[B] =
      foldRightL(l, Nil: List[B])((h, t) => Cons(f(h), t))

    def mapM[A, B](l: List[A])(f: A => B): List[B] = {
      val buf = new collection.mutable.ListBuffer[B]

      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h, t) => buf += f(h); go(t)
      }

      go(l)
      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }

    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    def filterL[A](l: List[A])(f: A => Boolean): List[A] =
      foldRightL(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    def filterM[A](l: List[A])(f: A => Boolean): List[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h, t) => if (f(h)) buf += h; go(t)
      }

      go(l)
      List(buf.toList: _*)
    }

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      concat(mapM(l)(f))

    def filterFM[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l)(a => if (f(a)) List(a) else Nil)

    def add(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add(t1, t2))
    }

    def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
      case _ => false
    }

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
  }

  def main(args: Array[String]): Unit = {
    println("sum " + List.sum(List(1, 2)))
    println("prod " + List.product(List(1, 2, 0)))
    println("lenght " + List.length(List(1, 2, 0)))

    println(List.sumL(List(1, 2)))
    println(List.productL(List(1, 2, 10.2)))
    println(List.lengthL(List(1, 2, 0, 9)))

    println(List.reverse(List(1, 2, 0, 9)))
    println("concat " + List.concat(List(List(1, 2, 0, 9), List(1, 2, 0, 9))))

    println(List.mapInt(List(1, 2, 0, 9)))
    println(List.mapDoubleString(List(1, 2, 0, 9)))

    println(List.map(List(1, 2, 0, 9))({
      _ + "a"
    }))
    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))
    println(List.filterFM(List(1, 2, 3))(_ < 2))
    println(List.add(List(1, 2, 3), List(1, 2, 3)))
    println(List.zipWith(List(1, 2, 3), List(1, 2, 3))(_ * _))
    println(List.hasSubsequence(List(1, 2, 3), List(3)))
  }
}
