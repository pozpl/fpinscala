package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1+ size(left) + size(right)
    }

    def max(t: Tree[Int]): Int = t match {
        case Leaf(a) => a
        case Branch(left, right) => max(left).max(max(right))
    }

    def depth[A](t:Tree[A]):Int = t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

    def map[A,B](t:Tree[A])(f: A=>B):Tree[B] = t match {
        case Leaf(a) => Leaf(f(a))
        case Branch(a,b) => Branch(map(a)(f), map(b)(f))
    }

}