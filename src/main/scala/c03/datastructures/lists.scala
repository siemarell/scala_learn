package c03.datastructures
import annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case 0 => l
      case _ => l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n-1)
      }
    }
  }
  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (!f(x)) Cons(x,xs)
        else dropWhile(xs)(f)
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_,xs) => Cons(h, xs)
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, xs) => Cons(h,init(xs))
    }
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    var temp = new ListBuffer[A]
    @annotation.tailrec
    def go(l: List[A]): List[A] = {
      l match {
        case Cons(_, Nil) => List(temp.toList: _*)
        case Cons(h, tail) => temp += h; go(tail)
      }
    }
    go(l)
  }


  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, xs) => foldLeft(xs, f(z,h))(f)
    }

  def sum2(l: List[Int]): Double =
    foldRight(l, 0.0)(_ + _)
  def product2(l: List[Double]): Double =
    foldRight(l, 1.0)(_ * _)

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc)=>acc+1)
  }

  def sum3(l: List[Int]): Int = {
    foldLeft(l,0)(_+_)
  }
  def product3(l: List[Double]): Double = {
    foldLeft(l,0.0)(_ * _)
  }

  def length3[A](l : List[A]):Int = {
    foldLeft(l,0)((acc, _) => acc + 1)
  }

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => foldRight(reverse(t), Cons(h,Nil))(Cons(_,_))
  }
  def reverseViaFoldLeft[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
  }
//  1 list[1,2,3] [] -> Cons(1,[])
  def foldRightViaFoldLeft[A,B](l: List[A], z:B)(f: ((A, B) => B)): B = {
    foldLeft(reverse(l),z)((a,b)=>f(b,a))
  }
//  def foldLeftViaFoldRight[A,B](l: List[A], z:B)(f: (B,A) => B): B = {
//
//  }

  def appendVal[A](l: List[A], i: A): List[A] = l match {
    case Nil => Cons(i, Nil)
    case Cons(h,t) => foldRight(Cons(h,t), Cons(i,Nil))(Cons[A])
  }

  def appendVal[A](l: List[A], l2: List[A]): List[A] = {
    foldRight(l,l2)(Cons(_,_))
  }

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(appendVal)

  def addOne(l: List[Int]):List[Int] = {
    foldRight(l, Nil:List[Int])((h,t)=>Cons(h+1,t))
  }
  def dbToStr(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((h,t)=>Cons(h.toString, t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
  }

  def map2[A,B](l: List[A])(f: A => B): List[B] = {
    import collection.mutable.ListBuffer
    var buf = new ListBuffer[B]
    @annotation.tailrec
    def go(l: List[A]): Unit = l match {
      case Nil => Unit
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf:_*)
  }
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) {
      Cons(h, t)
    } else {
      t
    })
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    flatten(map(l)(f))
  }
  def filterViaFlatMap[A](l: List[A])(f: A=> Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def addElements(l1: List[Int], l2: List[Int]): List[Int] = (l1,l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addElements(t1,t2))
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1,l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
  def main(args: Array[String]): Unit = {
    val example: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
    val example2: List[Int] = List(1,2,3)
    val example3: List[Int] = List(2,3,4,5,8,9,10)
    val total: Int = sum(example)
    println(example, example2, total)
    println(List.tail(Nil))
    println(List.drop(example,2))
    println(List.dropWhile(example2)(_<2))
    println(List.init(example2))
    println(List.init2(example2))
    println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println(reverse(example3))
    println(appendVal(example3, 120))
    println(List(example,example2))
    println(appendVal(example,example3))
    println(addOne(example2))
    println(dbToStr(List(2.0,3.3)))
    println(map(example3)(_+10))
    println(filter(example3)(_%2==0))
    println(flatMap(List(1,2,3))(i => List(i,i)))
  }

}