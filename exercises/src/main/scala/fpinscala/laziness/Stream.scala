package fpinscala.laziness

import Stream._

trait Stream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
            case _ => z
        }

    def exists(p: A => Boolean): Boolean =
        foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
        case Empty => None
        case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

    def toList(): List[A] = {
        def go(tail: Stream[A], acc: List[A]): List[A] = {
            tail match {
                case Cons(h, t) => go(t(), h() :: acc)
                case Empty => acc
            }
        }

        go(this, List[A]()).reverse
    }

    def take(n: Int): Stream[A] = this match{
        case Cons(h,t) if n > 1  => cons(h(), t().take(n-1))
        case Cons(h,_) if n == 1  => cons(h(), empty)
        case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
        case Cons(h,t) if n > 0 => t().drop(n-1)
        case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
        case _=>empty
    }

    def takeWhileViaFold(p:A => Boolean): Stream[A] =  {
        this.foldRight(empty:Stream[A])((a,b)=>if(p(a)) cons(a, b) else empty)
    }

    def forAll(p: A => Boolean): Boolean = {
        this.foldRight(true)((a,b) => p(a) && b )
    }

    def headOption: Option[A] = {
        foldRight(None: Option[A])((a,b) => Some(a))
    }

    // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
    // writing your own function signatures.

    def map[B](f: A=>B):Stream[B] = this match {
        case Cons(h,t) => cons(f(h()), t().map(f))
        case _ => empty
    }

    def map2[B](f: A => B):Stream[B] =
        foldRight(empty[B])((h,t) => cons(f(h), t))

    def filter(p:A=>Boolean):Stream[A] = this match {
        case Cons(h,t) => if(p(h()) ) cons(h(), t().filter(p)) else t().filter(p)
        case empty => empty
    }

    def filter2(p:A=>Boolean):Stream[A] = foldRight(empty: Stream[A])((x,acc) => if(p(x)) cons(x, acc) else acc )

    def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

    def from(n: Int): Stream[Int] = sys.error("todo")

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}