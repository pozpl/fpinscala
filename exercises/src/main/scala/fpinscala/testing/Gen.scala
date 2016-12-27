package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness.Stream
import fpinscala.state._
import Gen._
import Prop._
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par


/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


case class Prop(run: (MaxSize, TestCases,RNG) => Result) {
//  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  def &&(p:Prop):Prop = {
    Prop((m:MaxSize, n:TestCases, r:RNG) => {
        this.run(m, n, r) match {
          case Passed | Proved => p.run(m, n,r)
          case Falsified(failure, successes) => Falsified(failure, successes)
        }
    })
  }

  def ||(p:Prop):Prop = Prop {
    (m:MaxSize, n: TestCases, r: RNG) => {
      run(m, n, r) match {
        case Passed => Passed
        case Proved => Proved
        case Falsified(_, _) => p.run(m, n, r)
      }
    }
  }


}

object Prop {

  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"



  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m,n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }



}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State( (r:RNG )=> (a, r) ))

  def boolean: Gen[Boolean] = Gen(State((r:RNG) => {
    val (i, nr) = r.nextInt
    (i> 0, nr)
  }))

  def double: Gen[Double] = Gen(State((r:RNG) => {
    RNG.double(r)
  }))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(x => start + ( x % (stopExclusive - start) ) ))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]]   = {
     Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap((b:Boolean) => if(b) g1 else g2)
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val gw1 = g1._2
    val gw2 = g2._2
    val gwnTh = gw1/(gw1 + gw2)


    double.flatMap((d:Double) =>{
        if(d <= gwnTh) g1._1 else g2._1
    })

  }

  def listOf1[A](g: Gen[A]):SGen[List[A]] = {
    SGen(n => listOfN(1 max n, g))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen((n) => Gen.listOfN(n, g))
  }

  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

}

case class Gen[+A](sample: State[RNG,A]){
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))


  def flatMap[B](f: A => Gen[B]): Gen[B] = {
     Gen(sample.flatMap( a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  def unsized: SGen[A] = {
    SGen{
      n:Int => this
    }
  }

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}
case class SGen[+A](forSize: Int => Gen[A]){

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen {
    (n:Int) => {
      forSize(n).map(f)
    }
  }
  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen{
    (n:Int) => {
      forSize(n).flatMap(f)
    }
  }


}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}

//trait SGen[+A] {
//
//}

