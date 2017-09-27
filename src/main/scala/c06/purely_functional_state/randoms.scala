package c06.purely_functional_state
import annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}
object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }
  def randomPair(rng: RNG): (Int,Int) = {
    val (i1,_) = rng.nextInt
    val (i2,_) = rng.nextInt
    (i1,i2)
  }
  def randomPair2(rng: RNG): ((Int,Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }
  def positiveInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, y) => (0,y)
      case (x, y) => (x.abs, y)
    }
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }
  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  def doubleMy(rng: RNG): (Double, RNG) = {
    val res = positiveInt(rng)
    (res._1/Int.MaxValue.toDouble, res._2)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def _double(): Rand[Double] = map(int)(_ / Int.MaxValue.toDouble)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (d1, rng3) = double(rng2)
    ((i1,d1), rng3)
  }
  def intDoubleViaMap2(): Rand[(Int,Double)] = map2(int,_double())((a,b) => (a,b))

  def doubleIntViaMap2(): Rand[(Double, Int)] = map2(int,_double())((a,b) => (b,a))

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i1,d1),rng2) = intDouble(rng)
    ((d1,i1),rng2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val ((d1,i1),rng2) = doubleInt(rng)
    val ((d2,i2),rng3) = doubleInt(rng2)
    ((d1,d2,i1/Int.MaxValue.toDouble),rng3)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    import scala.collection.mutable.ListBuffer
    var l = new ListBuffer[Int]
    @tailrec
    def go(count: Int, rng: RNG): RNG = count match {
      case 0 => rng
      case n => {
        val (i, rng1) = rng.nextInt
        l += i
        go(n-1, rng1)
      }
    }
    val lastrng = go(count,rng)
    (l.toList, lastrng)
  }
  // A tail-recursive solution
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  }
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))


  def unit[A](a: A): Rand[A] =  rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }
  def positiveMax(n: Int): Rand[Int] = {
    map(int)(a => a * n/ Int.MaxValue)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A,B)=>C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b=> f(a,b)))
  def positiveInt2(): Rand[Int] = {
    flatMap(int)(a => if (a < 0) positiveInt2() else unit(a))
  }
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((h, z) => map2(h,z)(_ :: _))
//  def mapG[S,A,B](a: S => (A,S))(f: A => B): S => (B,S) = rng => {
//    val (b, s) = a(rng)
//    (f(a), s)
//  }
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

}
case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B):State[S, B] = State(s => {
    val (a, s1) = run(s)
    (f(a), s1)
  })

  def map1[B](f: A => B):State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A,B) => C): State[S, C] = {
    flatMap(a => sb map(b => f(a,b)))
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s) // (unit, state)
    f(a).run(s1)
  })
//_ => State.get[Machine] State.get = s => (s,s) s => (s, s.coins)
}

object State {
  //type State[S,+A] = S => (A,S)
  def unit[S, A](a: A): State[S,A] = State(s => (a,s))
  type Rand[A] = State[RNG, A]
  val twenty: Rand[Int] = State(rng => rng.nextInt)
  val int: Rand[Int] = State(_.nextInt)
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
  def map2[S,A,B,C](sa: State[S, A], sb: State[S, B])(f: (A,B) => C): State[S, C] = {
    sa flatMap(a => sb map(b => f(a,b)))
  }
  def sequenceViaFoldRight[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((h, z) => map2(h,z)(_ :: _))

  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  def main(args: Array[String]): Unit = {
    val a = int.flatMap(x =>
      int.flatMap(y =>
        intsViaSequence(x).map(xs =>
          xs.map(_ % y))))
    val b = for {
      x <- int
      y <- int
      xs <- intsViaSequence(x)
    } yield xs.map(_ % y)
    //println(a.run(RNG.simple(12)))
  }
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def modify2[S](f: S => S): State[S, Unit] = State(
    s1 => {
      val s2 = f(s1)
      ((),s2)
    }
  )
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
object randoms {
  def main(args: Array[String]): Unit = {
    val myRng = RNG.simple(-10)
    print("random pair1: ")
    println(RNG.randomPair(myRng))
    print("random pair2: ")
    println(RNG.randomPair2(myRng))
    print("random double: ")
    println(RNG.double(myRng))
    print("random int double: ")
    println(RNG.intDouble(myRng))
    print("random double int: ")
    println(RNG.doubleInt(myRng))
    print("random double3")
    println(RNG.double3(myRng))
    print("random ints")
    println(RNG.ints(10)(myRng))
    print("random ints2")
    println(RNG.ints(10)(myRng))
    print("IntDoubleViaMap2")
    println(RNG.intDoubleViaMap2()(myRng))
    println(RNG.positiveInt2()(myRng))
    println(RNG.map2ViaFlatMap(RNG.int, RNG.int)(_ + _)(myRng))
  }
}
sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int){

}
object candy {
  def update(i: Input)(s: Machine): Machine = (i, s) match {
    case (_, Machine(_,0,_)) | (Coin, Machine(false,_,_)) | (Turn, Machine(true,_,_)) => s
    case (Coin, Machine(true,can,coi)) => Machine(locked = false,can,coi+1)
    case (Turn, Machine(false,can,coi)) => Machine(locked = true,can-1,coi)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(inpt => State.modify[Machine](update(inpt))))
    s <- State.get[Machine]
  } yield (s.coins, s.candies)

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
     State.sequence(inputs.map(input => State.modify[Machine](update(input)))). // s => (s, List[Unit])
       flatMap(_ => State.get[Machine].map(s => (s.coins,s.candies)))
  }
}

