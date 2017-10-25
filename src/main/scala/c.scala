import scala.collection.mutable.ListBuffer

object c {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    def setHead[A](xs: List[A], h: A): List[A] = xs match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }

    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => l
      }

    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }

    def init2[A](l: List[A]): List[A] = {
      val buf = new ListBuffer[A]

      @annotation.tailrec
      def go(cur: List[A]): List[A] = cur match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => List(buf.toList: _*)
        case Cons(h, t) => buf += h; go(t)
      }

      go(l)
    }
  }


  def main(args: Array[String]): Unit = {
    println(x)
    print("tail ")
    println(List.tail(List(1, 2, 3, 4, 5)))
    print("head ")
    println(List.setHead(List(1, 2, 3, 4, 5), 8))
    println(List.drop(List(1, 2, 3, 4, 5), 2))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => {x < 2 }))
    println(List.init(List(1, 2, 3, 4, 5)))
  }

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h
    case _ => 101
  }

}
