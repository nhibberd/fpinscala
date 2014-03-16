package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  // Exercise 26
  def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l,r) => maximum(l) max maximum(r)
  }

  // Exercise 27
  def depth[A](t: Tree[A]): Int = sys.error("todo")

  // Exercise 28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = sys.error("todo")

  // Exercise 29
  def fold[A,B](t: Tree[A], z: B)(f: (A,B) => B): B = sys.error("todo")

  def sizeViaFold[A](t: Tree[A]): Int = sys.error("todo")

  def maximumViaFold(t: Tree[Int]): Int = sys.error("todo")

  def depthViaFold[A](t: Tree[A]): Int = sys.error("todo")

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = sys.error("todo")





}
