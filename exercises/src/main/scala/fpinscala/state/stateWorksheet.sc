import fpinscala.state._

val rngs = RNG.Simple(23)
RNG.nonNegativeInt(rngs)
RNG.ints(3)(rngs)
RNG.doubleViaMap()(rngs)
RNG.int(rngs)
RNG.ints(3)(rngs)
RNG.ints2(3)(rngs)
RNG.ints2v2(3)(rngs)




