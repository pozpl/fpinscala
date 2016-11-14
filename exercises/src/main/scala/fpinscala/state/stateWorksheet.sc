import fpinscala.state._

val rngs = RNG.Simple(23)
RNG.nonNegativeInt(rngs)
RNG.ints(3)(rngs)
RNG.doubleViaMap()(rngs)
RNG.int(rngs)
RNG.ints(3)(rngs)
RNG.ints2(3)(rngs)
RNG.ints2v2(3)(rngs)
RNG.nonNegativeLessThen(3)(rngs)
RNG.map(RNG.unit(2))((x:Int) => x)(rngs)
RNG.mapViaFlatMap(RNG.unit(2))((x:Int) => x)(rngs)
State.simulateMachine(List(Coin, Turn, Turn, Coin)).run(Machine(false, 3, 0))
State.simulateMachineSeq(List(Coin, Turn, Turn, Coin)).run(Machine(false, 3, 0))




