package c02.introduction

import scala.annotation.tailrec

object tail_recursion {
  def main(args: Array[String]): Unit = {
    //println(factorial(1))
    println(fib(7))
    println(fibLiteral.apply(7))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int):Int = {
      if (n == 1) acc else go(n-1, n*acc)
    }
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(x: Int, prev :Int = 0, next: Int = 1): Int = x match {
      case 0 => prev
      case 1 => next
      case _ => go(x-1, next, prev + next)
    }
    go(n)
  }

  val fibLiteral: (Int)=>(Int) =(n:Int)=> {
    @tailrec
    def go(x: Int, prev :Int = 0, next: Int = 1): Int = x match {
      case 0 => prev
      case 1 => next
      case _ => go(x-1, next, prev + next)
    }
    go(n)
  }
}
