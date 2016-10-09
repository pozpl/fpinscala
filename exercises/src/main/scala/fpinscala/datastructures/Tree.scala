package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
    }

    def max(t: Tree[Int]): Int = t match {
        case Leaf(a) => a
        case Branch(left, right) => max(left).max(max(right))
    }

    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(a) => Leaf(f(a))
        case Branch(a, b) => Branch(map(a)(f), map(b)(f))
    }

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
        case Leaf(a) => f(a)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }


    def sizeF[A](t: Tree[A]): Int = {
        fold(t)(_ => 1)((a, b) => 1 + a + b)
    }

    def maxF(t:Tree[Int]):Int = fold(t)(a=>a)((a,b) => a.max(b))

    def depthF[A](t:Tree[A]):Int = fold(t)(a=>1)((a,b) => 1+ a.max(b))
}