package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  //def &&(p:Prop):Prop = if(check) p else this;
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???


}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State( (r:RNG )=> (a, r) ))

  def boolean: Gen[Boolean] = Gen(State((r:RNG) => {
    val (i, nr) = r.nextInt
    (i> 0, nr)
  }))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(x => start + ( x % (stopExclusive - start) ) ))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]]   = {
     Gen(State.sequence(List.fill(n)(g.sample)))
  }

}

case class Gen[A](sample: State[RNG,A]){
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
     Gen(sample.flatMap( a => f(a).sample))
  }


}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

