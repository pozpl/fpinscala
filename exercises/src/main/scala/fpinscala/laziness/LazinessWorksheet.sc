import fpinscala.laziness._

val stream = Stream(1,3,5,22)
stream.toList
stream.take(2).toList()
stream.drop(2).toList()
stream.takeWhile(x => x < 3 ).toList()
stream.takeWhileViaFold(x => x < 6 ).toList()
stream.map((x) => x*2).toList()
stream.filter((x) => x%2 > 0).toList()
stream.filter2((x) => x%2 > 0).toList()
stream.append(Stream(4,4,4)).toList()
stream.append2(Stream(4,4,4)).toList()
stream.flatMap((x)=>Stream(x)).toList()
Stream.constant(2).take(5).toList()
Stream.from(2).take(5).toList()
Stream.unfold(stream)((s:Stream[Int]) => s match {
    case Cons(h, t) => Some(h(), t())
    case _ => None
}).toList()
Stream.constUnfold(2).take(5).toList()
Stream.fibsUnfold().take(5).toList()
stream.mapViaUnfold((_*3)).toList()
stream.takeViaUnfold(2).toList()
stream.takeWhileViaUnfold(x => x < 6 ).toList()

val secondStream:Stream[Int] = Stream(3,3,3)

stream.zipWithViaUnfold(secondStream, (a:Int,b:Int) => a+b ).toList()

stream.zipAll(secondStream).toList()

stream.startsWith(Stream(1,3))
stream.startsWith(Stream(2,3))
secondStream.tails().toList()










