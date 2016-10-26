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




