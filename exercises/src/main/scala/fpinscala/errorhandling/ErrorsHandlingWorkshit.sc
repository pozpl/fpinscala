import fpinscala.errorhandling._
val s = Some(12)
val n = None
s.map((a:Int)=>(a+2))
s.getOrElse(33)
s.orElse(Some(23))
s.flatMap((a)=>Some(a))
n.map((a:Int)=>(a+2))
n.orElse(Some(23))


val optl = Some(23)::Some(12)::Some(1)::Nil
Option.sequence(optl)
Option.sequenceViaTraverse(optl)

val right = Right(33)
val left:Either[String, Int] = Left[String]("Oi")

right.map((_+1))
left.map((_+1))

right.flatMap(a=>Right(a+1))
left.flatMap(a=>Right(a+1))
