

object e {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }

    def maximun(t: Tree[Int]): Int =
      t match {
        case Leaf(n) => n
        case Branch(l, r) => maximun(l) max maximun(r)
      }

    def deph[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 0
        case Branch(l, r) => 1 + (deph(l) max deph(r))
      }


    def map[A, B](l: Tree[A])(f: A => B): Tree[B] =
      l match {
        case Leaf(n) => Leaf(f(n))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }

    def fold[A, B](l: Tree[A])(f: A => B)(g: (B, B) => B): B =
      l match {
        case Leaf(n) => f(n)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      }

    def sizeViaFold[A](t: Tree[A]): Int =
      fold(t)(a => 1)(1 + _ + _)

    def maximumViaFold(t: Tree[Int]): Int =
      fold(t)(a => a)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int =
      fold(t)(a => 0)((l, r) => 1 + (l max r))

  }


  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf(9), Leaf(1)), Branch(Leaf(2), Branch(Leaf(10), Leaf(0))))
    println("size: " + Tree.size(tree))
    println("maximun: " + Tree.maximun(tree))
    println("deph: " + Tree.deph(tree))
    println("map: " + Tree.map(tree)(x => x.toString))
  }
}