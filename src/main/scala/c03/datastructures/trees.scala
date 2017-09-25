package c03.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A=>B)(g: (B,B) => B):B = t match {
    case Leaf(n) => f(n)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)((_)=>1)((a,b) => 1 + a + b) // ((1 + _ + _))

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a=>a)((b,c)=> b max c) // (_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_=>0)((a, b) => 1 + (a max b))

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

  def main(args: Array[String]): Unit = {
    val mytree = Branch(Branch(Leaf(1),Leaf(10)),Leaf(3))
    println(Tree.size(mytree))
    println(Tree.sizeViaFold(mytree))
    println(Tree.maximum(mytree))
    println(Tree.maximumViaFold(mytree))
    println(Tree.depth(mytree))
    println(Tree.depthViaFold(mytree))
    println(Tree.map(mytree)(_*10))
    println(Tree.mapViaFold(mytree)(_*10))
  }
}

