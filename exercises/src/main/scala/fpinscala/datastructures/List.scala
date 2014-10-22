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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case _ => Cons(h, l)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    n match {
      case 0 => l
      case _ => drop(tail(l), n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => y + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumLeft(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, List[A]())((acc, x) => Cons(x, acc))
    //foldLeft(xs, Nil)((acc, x) => Cons(x, acc))

  def appendRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, acc) => Cons(x, acc))

  def addOne(xs: List[Int]): List[Int] =
    foldRight(xs, List[Int]())((x, acc) => Cons(x + 1, acc))
    //List.reverse(foldLeft(xs, List[Int]())((acc, a) => Cons(a + 1, acc)))

  def toStrings(xs: List[Double]): List[String] =
    foldRight(xs, List[String]())((x, acc) => Cons(s"[...${x.toString}...]", acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((x, acc) => Cons(f(x), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = ???
}
object Lists {

  def main(args: Array[String]): Unit = {
    val ls = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val xs = Cons(5, Cons(6, Cons(7, Cons(8, Nil))))
    val lsDoubles = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Nil))))
    val mappedDoubles = List.map(ls){a: Int => a.toDouble}
    println(s"tail: ${List.tail(ls)}")
    println(s"setHead: ${List.setHead(ls, 0)}")
    println(s"drop: ${List.drop(ls, 2)}")
    val dw = List.dropWhile(ls, {a: Int => a % 2 == 1} )
    println(s"dropWhile: $dw")
    println(s"init: ${List.init(ls)}")
    println(s"length: ${List.length(ls)}")
    println(s"sum: ${List.sum(ls)}")
    println(s"sum2: ${List.sum2(ls)}")
    println(s"sum with foldLeft: ${List.sumLeft(ls)}")
    println(s"product: ${List.product(lsDoubles)}")
    println(s"product2: ${List.product2(lsDoubles)}")
    println(s"product with foldLeft: ${List.productLeft(lsDoubles)}")
    println(s"reverse: ${List.reverse(ls)}")
    println(s"append: ${List.append(ls, xs)}")
    println(s"appendLeft: ${List.appendRight(ls, xs)}")
    println(s"addOne: ${List.addOne(ls)}")
    println(s"toStrings: ${List.toStrings(lsDoubles)}")
    println(s"mapped: $mappedDoubles")
  }
}
