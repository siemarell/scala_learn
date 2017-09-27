package c05.strickness_laziness
import annotation.tailrec
trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toListRecursive: List[A] = uncons match {
    case None => Nil
    case Some((h, t)) => h :: t.toListRecursive
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s.uncons match {
      case Some((h,t)) => go(t, h :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s.uncons match {
      case Some((h,t)) =>
        buf += h
        go(t)
      case _ => buf.toList
    }
    go(this)
  }
  def take(n: Int): Stream[A] = {
    if (n == 0) Stream.empty
    else uncons match {
      case None => Stream.empty
      case Some((h, t)) => Stream.cons(h, t.take(n-1))
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    uncons match {
      case Some((h,t)) if p(h) => Stream.cons(h, t.takeWhile(p))
      case _ => Stream.empty
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A])((a,b) => if (p(a)) Stream.cons(a, b.takeWhile(p)) else b)


  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) & b)

  def map[B](p: A =>B ): Stream[B] =
    foldRight(Stream.empty: Stream[B])((value, acc) => Stream.cons(p(value), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((value, acc) => if (p(value))  Stream.cons(value, acc) else acc)

  def append[B>:A](bb: => Stream[B]): Stream[B] =
    foldRight(bb)(Stream.cons(_,_))

  def flatMap[B >:A](f: B=> Stream[B]): Stream[B] =
    foldRight(Stream.empty: Stream[B])((a,b) => f(a).append(b))
}
object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fibs(): Stream[Int] = {
    def go(prev: => Int, next: => Int): Stream[Int] = {
      cons(next,go(next, next+ prev))
    }
    go(0,1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty
    case Some((h,t)) => cons(h, unfold(t)(f))
  }
  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])((p: (A,S)) => cons(p._1,unfold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      //f(z).map((p: (A,S)) => cons(p._1,unfold(p._2)(f))).getOrElse(empty[A])
    f(z).map((zz: (A, S)) => cons(zz._1, unfoldViaMap(zz._2)(f))).getOrElse(Stream.empty)

  def fibsViaUnfold(): Stream[Int] =
    unfold((0,1))((tt)=>Some((tt._2,(tt._2, tt._1 + tt._2))))

  def fromViaUnfold(n: Int) =
    unfold(n)(n => Some((n,n+1)))

  def onesViaUnfold(): Stream[Int] =
    unfold(1)(_=> Some(1,1))
  def constantViaUnfold(n: Int): Stream[Int] =
    unfold(n)(_ => Some((n,n)))

//  def map[A,B](a: Stream[A])(f: A=>B): Stream[B] = a uncons match {
//    case None unfold()
//  }
  def main(args: Array[String]): Unit = {
    val myStream = Stream(1,2,3,4,5,6)
    println(myStream)
    println(myStream.toList)
    println(myStream.take(3).toList)
    println(myStream.takeWhile(_<4).toList)
    println(myStream.forAll(_ >4))
    println(myStream.forAll(_ <10))
    println(constant(10).filter(_ %2 == 0).take(20).toList)
    println(from(10).filter(_ %2 == 0).take(20).toList)
  }

}