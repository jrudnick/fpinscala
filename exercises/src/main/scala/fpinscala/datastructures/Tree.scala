package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](xs: Tree[A]): Int = xs match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def max(xs: Tree[Int]): Int = xs match {
    case Leaf(v) => v
    case Branch(left, right) => max(left).max(max(right))
  }

  def depth[A](xs: Tree[A]): Int = xs match {
    case Leaf(_) => 1
    case Branch(left, right) => depth(left).max(depth(left)) + 1
  }

  def map[A, B](xs: Tree[A])(f: A => B): Tree[B] = xs match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

}
object Trees {

  def main(args: Array[String]): Unit = {
    val t: Tree[Int] = Branch(Branch(Leaf(4), Leaf(2)), Branch(Leaf(8), Leaf(1)))
    val mapped = Tree.map(t) { _ * 2 }
    println(s"size: ${Tree.size(t)}")
    println(s"max: ${Tree.max(t)}")
    println(s"depth: ${Tree.depth(t)}")
    println(s"map: $mapped")
  }
}
