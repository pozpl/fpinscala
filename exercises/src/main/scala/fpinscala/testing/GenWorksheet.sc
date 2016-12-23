import fpinscala.state._
import fpinscala.testing._

val rngs = RNG.Simple(23)
Gen.boolean.sample.run(rngs)







