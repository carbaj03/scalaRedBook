import java.util.concurrent.TimeUnit

import scala.concurrent.duration.TimeUnit

object chapter7 {

  trait ExecutorService {
    def submit[A](a: Callable[A]): Future[A]
  }

  trait Callable[A] {
    def call: A
  }

  trait Future[A] {
    def get: A

    def get(timeout: Long, unit: TimeUnit): A

    def cancel(evenIfRunning: Boolean): Boolean

    def isDone: Boolean

    def isCancelled: Boolean
  }



  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      es => {
        val (af, bf) = (a(es), b(es))
        Map2Future(af, bf, f)
      }

    case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                   f: (A, B) => C) extends Future[C] {
      @volatile var cache: Option[C] = None

      def isDone =
        cache.isDefined

      def isCancelled =
        a.isCancelled || b.isCancelled

      def cancel(evenIfRunning: Boolean) =
        a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

      def get =
        compute(Long.MaxValue)

      def get(timeout: Long, units: TimeUnit): C =
        compute(TimeUnit.NANOSECONDS.convert(timeout, units))

      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime;
          val aTime = stop - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }

      def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
          def call = a(es).get
        })

      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

      def asyncF[A, B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))

      def map[A, B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a, _) => f(a))

      def sortPar(parList: Par[List[Int]]) =
        map(parList)(_.sorted)

      def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
        l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

      def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
        as match {
          case Nil => unit(Nil)
          case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
        }

      def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
        if (as.isEmpty) unit(Vector())
        else if (as.length == 1) map(as.head)(a => Vector(a))
        else {
          val (l, r) = as.splitAt(as.length / 2)
          map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
        }
      }

      def sequence[A](as: List[Par[A]]): Par[List[A]] =
        map(sequenceBalanced(as.toIndexedSeq))(_.toList)

      def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
      }

      def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
        val pars: List[Par[List[A]]] =
          l map asyncF((a: A) => if (f(a)) List(a) else List())
        map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
      }
    }

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => {
        val ind = run(es)(n).get // Full source files
        run(es)(choices(ind))
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))


    def join[A](a: Par[Par[A]]): Par[A] =
      es => run(es)(run(es)(a).get())
  }

}
