package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0)((x,y) => x + y)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`, see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,t) => t
  }

  def setHead[A](l: List[A])(h: A): List[A] =
    Cons(h, l)


  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop (tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_,t) => init(t)
    case _ => l
  }

  // Exercise 9
  def length[A](l: List[A]): Int =
    foldRight(l,0)((_,i) => i + 1)

  // Exercise 10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z,h))(f)
  }

  // Exercise 11
  def sum3(l: List[Int]) = foldLeft(l,0)(_+_)
  def product3(l: List[Double]) = foldLeft(l,1.0)(_*_)
  def length2[A](l: List[A]): Int = foldLeft(l,0)((acc,_) => acc + 1)

  // Exercise 12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l,Nil:List[A])((acc,h) => Cons(h,acc))

  // Exercise 13
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc,h) => f(h,acc))

  // Exercise 14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h,acc) => Cons (h, acc))
    // (h,acc) => Cons(h,acc)
    // (_,_) => Cons(_,_)

  // Exercise 15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l,Nil:List[A])((h,acc) => append(h,acc))
    // ((h,acc) => append(h,acc))
    // append

  // Exercise 16
  def add1(l: List[Int]): List[Int] =
    foldRight(l,Nil:List[Int])((h,acc) => Cons(h + 1, acc))

  // Exercise 17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l,Nil:List[String])((h,acc) => Cons(h.toString, acc))

  // Exercise 18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,acc) => Cons(f(h),acc))

  // Exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,acc) => if (f(h)) Cons(h,acc) else acc)

  // Exercise 20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil:List[B])((h,acc) => append(f(h),acc))
    //concat(map(l)(f))

  // Exercise 21
  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  // Exercise 22
  def addPair(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x,xs), Cons(y,ys)) => Cons(x+y, addPair(xs,ys))
  }

  // Exercise 23 -- zipWith
  def addPair_2[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), addPair_2(xs,ys)(f))
  }

  // Exercise 24
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = sys.error("")
}
