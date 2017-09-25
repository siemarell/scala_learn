package c02.introduction
import scala.annotation.tailrec


object polyfunctions {
  def main(args: Array[String]): Unit = {
    val a = Array(1,2,3,4)
    val b = Array(1,2,1,5)
    println(isSorted(a, (x: Int, y:Int)=> x>y))
    println(isSorted(b, intGT))

    val prependOne = partial1(1, concatIntAndString)
    println(prependOne("-something"))
    val curriedConcatIntAndString = curry(concatIntAndString)
    println(curriedConcatIntAndString(2)("-something"))
    val uncurriedConcatIntAndString = uncurry(curriedConcatIntAndString)
    println(uncurriedConcatIntAndString(3, "-something"))
  }

  val intGT: (Int, Int)=> Boolean = (x,y)=> x>y
  def isSorted[T](array: Array[T], gt: (T, T) => Boolean): Boolean = {
    val arrLen = array.length
    @tailrec
    def go(step: Int): Boolean = {
      if (step == arrLen - 1) true
      else if (!gt(array(step + 1), array(step))) false
      else go(step + 1)
    }
    arrLen match {
      case 0 => true
      case 1 => true
      case _ => go(0)
    }
  }

  def concatIntAndString(a: Int, s: String): String = {
    a.toString + s
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (arg2: B) => f(a, arg2)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (arg1: A) => (arg2:B)=> f(arg1, arg2)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }
}
