package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    // `List` companion object. Contains functions for creating and working with lists.
    def sum(ints: List[Int]): Int = ints match {
        // A function that uses pattern matching to add up a list of integers
        case Nil => 0 // The sum of the empty list is 0.
        case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
        a1 match {
            case Nil => a2
            case Cons(h, t) => Cons(h, append(t, a2))
        }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


    def tail[A](l: List[A]): List[A] = l match {
        case Cons(h, t) => t
        case Nil => Nil
    }

    def setHead[A](l: List[A], h: A): List[A] = {
        Cons(h, tail(l))
    }

    def drop[A](l: List[A], n: Int): List[A] = {
        if (n > 0) {
            l match {
                case Cons(h, t) => drop(t, n - 1)
                case Nil => Nil
            }
        } else {
            l
        }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        l match {
            case Cons(h, t) if f(h) => dropWhile(t, f)
            case _ => l
        }
    }

    def init[A](l: List[A]): List[A] = l match {
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
        case _ => Nil
    }

    def length[A](l: List[A]): Int = {
        foldRight(l, 0)((x, y) => y + 1)
    }

    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
        l match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }
    }

    def sumFl(l: List[Int]): Int = {
        foldLeft(l, 0)(_ + _)
    }

    def lengthFl[A](l: List[A]) = {
        foldLeft(l, 0)((x, y) => x + 1)
    }

    def productFl(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

    def reverse[A](l: List[A]): List[A] = foldLeft(l: List[A], List[A]())((accList: List[A], h: A) => Cons(h, accList))

    def foldLeftViaRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
        foldRight(reverse(l), z)((a: A, b: B) => f(b, a))
    }

    def foldRightViaLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a))

    def foldRightViaFoldLeft2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
        foldLeft(l, (b: B) => b)((af: B => B, r: A) => (b: B) => af(f(r, b)))(z)
    }

    def appendViaFoldL[A](l1: List[A], l2: List[A]): List[A] = {
        foldLeft(reverse(l1: List[A]), l2: List[A])((acc: List[A], a: A) => Cons(a, acc))
    }

    def flat[A](ll: List[List[A]]): List[A] = {
        foldLeft(ll, Nil: List[A])((l: List[A], r: List[A]) => appendViaFoldL(l, r))
    }

    def add1(l: List[Int]):List[Int] = reverse(foldLeft(l, Nil:List[Int])( (a:List[Int], h:Int) => Cons(h+1, a) ))

    def map[A, B](l: List[A])(f: A => B): List[B] = reverse(foldLeft(l, Nil:List[B])( (a:List[B], h:A) => Cons(f(h), a) ))

    def filter[A](l:List[A], p:A=>Boolean):List[A] = {
        reverse(foldLeft(l, Nil:List[A])( (a:List[A], h:A) => {
            if (p(h)) Cons(h, a)
            else a
        } ))
    }

    def flatMap[A,B](as:List[A])(f:A => List[B]):List[B] = flat(map(as)(f))

    def filterViaFlatMap[A](l:List[A], p:A=>Boolean):List[A] = {
        flatMap(l)((a) => if(p(a)) List(a) else List()  )
    }

    def zipWith[A,B](l1:List[A], l2:List[A], f:(A,A)=>B):List[B] =  {
        def go(l1:List[A], l2:List[A] ,acc:List[B]):List[B] = l1 match {
            case Nil => acc
            case Cons(h,t) => l2 match {
                case Cons(hl2, tl2) => go(t,tl2, Cons(f(h,hl2) ,acc))
                case Nil => acc
            }
        }

        reverse(go(l1, l2, List[B]()))
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        if(length(sup) >= length(sub)){
            if(sum(zipWith(sup, sub, (a:A,b:A) => if(a==b) 1 else 0)) == length(sub)) {
                true
            }else{
                hasSubsequence(tail(sup), sub)
            }
        }else{
            false
        }
    }

}
