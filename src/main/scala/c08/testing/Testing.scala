package c08.testing
import c06.purely_functional_state._
import c05.strickness_laziness._

import scala.collection.mutable
import Prop._
import Gen._
import c08.testing.Gen.Domain


trait Status
case object Proven extends Status
case object Unfalsified extends Status

case class Prop(run: (TestCases,RNG) => Result){
  def &&(p: Prop): Prop = Prop {
    (n, rng) => run(n,rng) match {
      case Right((status, count)) => p.run(n,rng).right.map { case (s,m) => (s,n+m) }
      case l => l
    }
  }
  def ||(p: Prop): Prop = Prop {
    (n, rng) => run(n,rng) match {
      case Left(_) => p.run(n, rng)
      case r => r
    }
  }
}

object Prop {
  type TestCases = Int
  type Result = Either[FailedCase, (Status,SuccessCount)]
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => {
      def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
        if (i == j) Right((Unfalsified, i))
        else s.uncons match {
          case Some((Some(h),t)) =>
            try { if (f(h)) go(i+1,j,s,onEnd)
              else Left(h.toString) }
            catch { case e: Exception => Left(buildMsg(h, e)) }
          case Some((None,_)) => Right((Unfalsified,i))
          case None        => onEnd(i)
        }
      go(0, n/3, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified,_)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n/3, n, rands, i => Right((Unfalsified, i)))
        case s => s
      }
    }
  }

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")
}

case class Gen[+A](sample: State[RNG,A], exhaustive: Domain[A]){
  def map[B](f: A => B): Gen[B] = Gen(
    sample.map(f), exhaustive.map(_ map f)
  )
  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = Gen(
    sample.map2(g.sample)(f), map2Stream(exhaustive,g.exhaustive)(map2Option(_,_)(f))
  )
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample),
    exhaustive.flatMap {
      case None => unbounded
      case Some(a) => f(a).exhaustive
    }
  )

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)
}


object Gen {
  type Domain[+A] = Stream[Option[A]]
  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))
  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), bounded(Stream(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean), bounded(Stream(false, true)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(a => start + a % (stopExclusive -start)),
      bounded(Stream.from(start).take(stopExclusive-start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)), Stream())

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((x, acc) => x.map2(acc)(_ :: _))

  def uniform: Gen[Double] = Gen(State(RNG.double), unbounded)

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(a => i + a*(j-i)),
      unbounded)
  //def forAll[A](gen: List[A])(f: A => Boolean): Prop =
  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 == 0) stopExclusive - 1 else stopExclusive).
      map (n => if (n%2 != 0) n+1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 != 0) stopExclusive - 1 else stopExclusive).
      map (n => if (n%2 == 0) n+1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int,Int)] = for {
    i <- choose(from,to)
    j <- if (i%2==0) even(from,to) else odd(from,to)
  } yield (i,j)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(x => if (x) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val threshold = g1._2 / (g1._2 + g2._2)
    uniform.flatMap(d => if (d < threshold) g1._1 else g2._1)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def main(args: Array[String]): Unit = {
    val myRng = RNG.simple(11223)
    val myRng1 = RNG.simple(212312)
//    println(choose(0,100).run(myRng))
//    println(choose(-10,20).run(myRng1))
  }

  /* `map2Option` and `map2Stream`. Notice the duplication! */
  def map2Option[A,B,C](oa: Option[A], ob: Option[B])(f: (A,B) => C): Option[C] =
    for { a <- oa; b <- ob } yield f(a,b)

  /* This is not the same as `zipWith`, a function we've implemented before.
   * We are generating all (A,B) combinations and using each to produce a `C`.
   * This implementation desugars to sa.flatMap(a => sb.map(b => f(a,b))).
   */
  def map2Stream[A,B,C](sa: Stream[A], sb: => Stream[B])(f: (A,=>B) => C): Stream[C] =
    for { a <- sa; b <- sb } yield f(a,b)
}
case class SGen[+A](forSize: Int => Gen[A]) {

}