package c08.testing
import c06.purely_functional_state._
import c05.strickness_laziness._

import scala.collection.mutable
import Prop._
import Gen._
import c08.testing.Gen.Domain
/**
  * Created by siem on 25/09/2017.
  */

//sealed trait Gen[+A]

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}
case class Gen[+A](sample: State[RNG,A], exhaustive: Domain[A]){
  def map[B](f: A => B): Gen[B] = Gen(
    sample.map(f), exhaustive.map(_ map f)
  )
  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = Gen(
    sample.map2(g.sample)(f), map2Stream(exhaustive,g.exhaustive)(map2Option(_,_)(f))
  )
}

trait Prop { def check: Either[FailedCase,SuccessCount] }
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

  def uniform: Gen[Double] = Gen(State(RNG.double), unbounded)

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(a => i + a*(j-i)),
      unbounded)
  //def forAll[A](gen: List[A])(f: A => Boolean): Prop =

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
