package c04.errors_without_exceptions

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(b) => Left(b)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(smth) => f(smth)
    }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b map(bb => f(aa,bb)))

  def map2for[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {a <- this;b1 <- b} yield f(a,b1)
}

object test {
  def sequence3[E,A](a: List[Either[E,A]]): Either[E,List[A]] =
    traverse(a)(x => x)
  def traverse[E, A, B](a: List[A])(f: A => Either[E,B]): Either[E, List[B]] = a match {
    case Nil => Right(Nil): Either[E, List[B]]
    case h :: t =>  f(h) flatMap(hh => traverse(t)(f) map (hh :: _))
  }

  def traverse_2[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
}