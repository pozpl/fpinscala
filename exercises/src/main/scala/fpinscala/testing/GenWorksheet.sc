import fpinscala.state._
import fpinscala.testing._
import Prop._

val rngs = RNG.Simple(23)
Gen.boolean.sample.run(rngs)

val smallInt = Gen.choose(-10,10)
val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns:List[Int] =>
    val max = ns.max
    !ns.exists(_ > max)
}

Prop.run(maxProp)

val sortProp = Prop.forAll(Gen.listOf1(smallInt)){
    nl:List[Int] =>
        val ns = nl.sorted
        ! nl.isEmpty && ! ns.isEmpty && (ns.size == 1
            || ! (ns.take(ns.size -1).zip(ns.slice(1, ns.size))
            .exists{case (a:Int,b:Int) => a > b}))
}

Prop.run(sortProp)










