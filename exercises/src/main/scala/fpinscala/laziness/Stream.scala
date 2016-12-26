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

    def append[B>:A](s: =>Stream[B]): Stream[B] = this match {
        case Cons(h, t) => cons(h(), t().append(s))
        case _ => s
    }

    def append2[B>:A](s: =>Stream[B]): Stream[B] = this.foldRight(s)((x,a) => cons(x, a))

    def flatMap[B](f: A=> Stream[B]):Stream[B] = this.foldRight(empty[B])( (x,acc) => f(x).append(acc) )

    def mapViaUnfold[B](f:A => B):Stream[B] = {
        unfold(this)((s:Stream[A]) => s match {
            case Cons(h,t) => Some((f(h()), t()))
            case empty => None
        })
    }

    def takeViaUnfold(n: Int): Stream[A] = {
        unfold((this, n) )( (s) => s match {
            case (Cons(h,t), ni) => if(ni > 0) Some((h(), (t(), ni - 1))) else None
            case _ => None
        } )
    }

    def takeWhileViaUnfold(p:A => Boolean): Stream[A] = {
        unfold(this )( (s) => s match {
            case Cons(h,t) => if(p(h())) Some((h(), t())) else None
            case _ => None
        } )
    }

    def zipWithViaUnfold[B, C](s: Stream[B], f: (A,B) => C):Stream[C] = {
        unfold((this, s))( (pair) => pair match {
            case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
            case _ => None
        })
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
        unfold((this, s2))((pair)=> pair match {
            case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
            case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
            case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
            case _ => None
        })
    }


    // special case of `zipWith`
    def zip[B](s2: Stream[B]): Stream[(A,B)] =
        zipWithViaUnfold(s2, (a:A,b:B) =>(a,b))

    def startsWith[B](s: Stream[B]): Boolean = {
        unfold((this, s))((pair) => pair match {
            case (Cons(h1, t1), Cons(h2, t2)) => Some(h1() == h2(), (t1(), t2()))
            case (Empty, Cons(h2, t2)) => Some(false, (empty, t2()))
            case _ => None
        }).forAll((x:Boolean)=> x)
    }

    def tails():Stream[Stream[A]] = {
        unfold(this)((s) => s match {
            case Empty => None
            case s => Some(s, s.drop(1))
        }).append(Stream())
    }

    def scanRight[B](z:B)(f: (A,=> B) => B):Stream[B] = {
        this.foldRight((z, Stream(z)))((a,acc) => {
            lazy val accl = acc
            val b2 = f(a, accl._1)
            (b2, cons(b2, accl._2))
        })._2
    }
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

    def constant[A](a:A):Stream[A] = {
//        Stream.cons(a, constant(a))
        lazy val tail:Stream[A] = Cons(() => a, ()=>tail )
        tail
    }

    def from(n: Int): Stream[Int] = {
        cons(n, from(n+1))
    }

    def fibs():Stream[Int] = {
        def go(x:Int, y:Int):Stream[Int] = {
            val next = x + y
            cons(x, go(y, next))
        }
        go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
            case Some((a,s)) => cons(a, unfold(s)(f))
            case _ => empty
        }
    }

    def onesUnfold: Stream[Int] = unfold(1)((s) => Some(1, 1))

    def constUnfold(const: Int):Stream[Int] = unfold(const)((s) => Some(s, s))

    def fibsUnfold():Stream[Int] = unfold((0,1))((p:(Int, Int)) => Some(p._1, (p._2, p._1+p._2) ))
    def fromUnfold(x:Int):Stream[Int] = unfold(x)((a:Int) => Some(a, a+1 ))
}