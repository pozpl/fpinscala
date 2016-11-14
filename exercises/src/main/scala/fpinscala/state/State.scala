package fpinscala.state


trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

    // NB - this was called SimpleRNG in the book text

    case class Simple(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
            val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
            val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
            (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
        }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (n, nr) = rng.nextInt
        val posInt = if (n == Int.MinValue) {
            0
        }
        else if (n < 0) {
            -n
        }
        else {
            n
        }
        (posInt, nr)
    }

    def double(rng: RNG): (Double, RNG) = {
        val (nn, nrng) = nonNegativeInt(rng)
        return (nn.toDouble / (Int.MaxValue + 1), nrng)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, r1) = rng.nextInt
        val (d, r2) = double(r1)
        ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((i, d), r) = intDouble(rng)
        ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, r1) = double(rng)
        val (d2, r2) = double(rng)
        val (d3, r3) = double(rng)
        ((d1, d2, d3), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def go(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
            if (n > 0) {
                val (i, r) = rng.nextInt
                go(n - 1, i :: acc, r)
            } else {
                (acc.reverse, rng)
            }
        }

        go(count, List(), rng)
    }

    def doubleViaMap(): Rand[Double] = map(nonNegativeInt)((x: Int) => x / (Int.MaxValue.toDouble + 1))

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, ra1) = ra(rng)
            val (b, rb1) = rb(ra1)
            (f(a, b), rb1)
        }
    }

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

        fs match {
            case h :: t => {
                (rng:RNG) => {
                    val (a, r) = h(rng)
                    val (lt, rl) = sequence(t)(r)
                    (a::lt, rl)
                }
            }
            case _ => rng => (List[A](), rng)
        }
    }


    def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
        fs.foldRight((r:RNG) => (List[A](),r ))( (n,acc) => {
            map2(n,acc)(_::_)
        })
    }

    def ints2(count:Int) = {
        sequence(List.fill(count)(int))
    }
    def ints2v2(count:Int) = {
        sequence(List.fill(count)(int))
    }

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
        rnd => {
            val (a, r2) = f(rnd);
            g(a)(r2)
        }
    }

    def nonNegativeLessThen(n: Int): Rand[Int] = {
        flatMap(nonNegativeInt)(i => {
            val mod = i % n
            if (i + (n - 1) - mod >= 0)
                unit(mod)
            else nonNegativeLessThen(n)
        })
    }

    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
        flatMap(s)(i => unit(f(i)))
    }

    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        flatMap(ra)(a => {
            map(rb)( b => f(a,b) )
        })
    }

}

import State._

case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = {
        State((r:S) => {
            val (a,s) = run(r)
            (f(a), s)
        })
    }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
        State((r:S) => {
            val (a,s) = run(r)
            val (b, s2) = sb.run(s)
            (f(a,b), s2)
        })
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
        State((r:S) => {
            val (a,s) = run(r)
            f(a).run(s)
        })

    def mapViaFlatMap[B](f: A => B): State[S, B]  = {
        flatMap(a => unit(f(a)))
    }

    def map2ViaFlatMap[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
        this.flatMap(a => {
            sb.mapViaFlatMap(b => f(a,b))
        })
    }
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
    type Rand[A] = State[RNG, A]

    def unit[A,S](a:A):State[S, A] = State((s:S) => (a,s))

    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
        sas.foldRight(State((r:S) => (List[A](),r)))((a:State[S,A],acc:State[S, List[A]]) => {
            a.map2(acc)((a:A, la:List[A]) => a::la)
        })
    }

    def sequenceFl[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
        val ns:State[S,List[A]] = sas.foldLeft(State((r:S) => (List[A](),r)))((acc:State[S, List[A]],a:State[S,A]) => {
            a.map2(acc)((a:A, la:List[A]) => a::la)
        })

        State((s:S) => {
            val (l,s2)= ns.run(s)
            (l.reverse, s)
        })
    }

    def modify[S](f: S => S): State[S, Unit] = for {
        s <- get // Gets the current state and assigns it to `s`.
        _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))


    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
        State(initialState => {
            val finalMachine = inputs.foldLeft(initialState)((acc, i) => (i, acc) match {
                case (_, Machine(_, 0, _))  => acc
                case (Coin, Machine(false, can, coins)) => Machine(true, can, coins + 1)
                case (Coin, Machine(true, can, coins)) => Machine(true, can, coins)
                case (Turn, Machine(true, can, coins)) => Machine(false, can - 1 , coins)
                case (Turn, Machine(false, can, coins)) => Machine(false, can , coins)
            })
            finalMachine match {
                case Machine(_, can, coin) => ((can, coin), finalMachine)
            }
        })
    }

    def simulateMachineSeq(inputs: List[Input]): State[Machine, (Int, Int)] = {

        def update(acc:Machine, i:Input):Machine = (i, acc) match {
            case (_, Machine(_, 0, _))  => acc
            case (Coin, Machine(false, can, coins)) => Machine(true, can, coins + 1)
            case (Coin, Machine(true, can, coins)) => Machine(true, can, coins)
            case (Turn, Machine(true, can, coins)) => Machine(false, can - 1 , coins)
            case (Turn, Machine(false, can, coins)) => Machine(false, can , coins)
        }

        State(initialState => {
            val (_, finalMachine) = sequence(inputs.map( (x:Input) => modify[Machine]( (m:Machine) => update(m, x) ))).run(initialState)

            finalMachine match {
                case Machine(_, can, coin) => ((can, coin), finalMachine)
            }
        })
    }
}
