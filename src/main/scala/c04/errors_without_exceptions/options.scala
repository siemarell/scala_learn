package c04.errors_without_exceptions

/**
  * Created by siem on 07/09/2017.
  */
trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
   this.map(f).getOrElse(None)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object test1 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match{
    case (None, _) => None
    case (_, None) => None
    case (Some(x), Some(y)) => Some(f(x,y))
  }

  def map21[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil:List[A]):Option[List[A]])((h, z) => map2(h,z)(_ :: _))
  }

  def sequence3[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil): Option[List[B]]
    case h :: t =>  f(h) flatMap(hh => traverse(t)(f) map (hh :: _))
  }

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def main(args: Array[String]): Unit = {
    val testList = List(Some(1), Some(2),Some(10))
    val testList2 = List(2,6,4)
    println(sequence(testList))
    println(traverse(testList2)(x => if (x % 2 == 0) Some(x) else None))
  }
}