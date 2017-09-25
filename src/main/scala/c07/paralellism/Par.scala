package c07.paralellism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}

object Par2 {
  def main(args: Array[String]): Unit = {
    val is =Vector(1,2,3,4,5,6)
    val pool = Executors.newCachedThreadPool()
    println(Par.run(pool)(parOp(is)(_ + _)).get())
    println(Par.run(pool)(parOp(is)(_ max _)).get())
    val words = Vector("Cka bla", "Cyka tvou mat", "Xyu", "xep xep xep xep")
    println(Par.run(pool)(calcWords(words)))
    val a = fork(unit(1))
    val b = fork(unit(3))
    val c = fork(unit(5))
    println(Par.run(pool)(map3(a,b,c)(_+_*_)))
  }

  def calcWords(parag: IndexedSeq[String]): Par[Int] = {
    if (parag.isEmpty) unit(0)
    else if (parag.size == 1) {
      unit(parag.head.split(' ').length)
    }else{
      val (l, r) = parag.splitAt(parag.size/2)
      map2(Par.fork(calcWords(l)), Par.fork(calcWords(r)))(_+_)
    }
  }
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sum2(as: IndexedSeq[Int]): Par[Int] =
    if (as.isEmpty) Par.unit(0)
    else {
      val (l,r) = as.splitAt(as.length/2)
      Par.map2(Par.fork(sum2(l)), Par.fork(sum2(r)))(_ + _)
    }
  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def parOp(is: IndexedSeq[Int])(f: (Int, Int) => Int): Par[Int] = {
    //println(Thread.currentThread().getId)
    if (is.size <= 1) {
      unit(is.headOption.getOrElse(20))
    }
    else {
      val (l, r) = is.splitAt(is.length/2)
      Par.map2(Par.fork(parOp(l)(f)), Par.fork(parOp(r)(f)))(f)
    }
  }
  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (s: ExecutorService) => {
    UnitFuture(f(a(s).get,b(s).get))
  }

  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C)=>D): Par[D] = {
    val pbc = map2(pb, pc)((b, c) => (b, c))
    map2(pa,pbc)((a,bc) => f(a,bc._1,bc._2))
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(() => a(es).get)
  }

  def asyncF[A,B](f: A => B): A => Par[B] = a => fork(unit(f(a)))
  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    map2(l, unit(()))((a, _) => a.sorted)
  def sortPar2(l: Par[List[Int]]) = map(l)(_.sorted)
  def map[A,B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a,_) => f(a))

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(Nil))((value, acc) => map2(value, acc)(_::_))

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(l.toIndexedSeq))(_.toList)
  def parMapPrimitive[A,B](l: List[A])(f: A => B): Par[List[B]] = (es: ExecutorService) => {
    es.submit(new Callable[List[B]] {
      override def call(): List[B] = l map f
    })
  }
  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = {
    sequence(l.map(asyncF(f)))
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = l.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }
  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
  def choice_simple[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = es =>
    map(a)(a => if (a) ifTrue(es) else ifFalse(es))(es).get

//  def choiceN_simple[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
//    val n = a(es)
//    choices(n)(es)
//  }

  def flatMap[A,B](a: Par[A])(choices: A => Par[B]): Par[B] = es => {
    choices(a(es).get())(es)
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    flatMap(a)(a => if (a) ifTrue else ifFalse)

  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(a)(choices(_))

  def join[A](a: Par[Par[A]]): Par[A] = es =>
    a(es).get()(es)
}