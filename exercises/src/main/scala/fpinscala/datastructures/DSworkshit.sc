import fpinscala.datastructures._

val a:List[Int] = List(1,2,3,4)
val b = List(5,6,7,8)

List.sumFl(List(1,2,3,4))
List.productFl(List(1,2,3,4))
List.lengthFl(List(1,2,3,4))

List.appendViaFoldL(a,a)
List.flat(List(a,b,a))
List.add1(a)

List.filter(a, (a:Int) => a % 2 == 1)

List.flatMap(a)((x)=>List(x))
List.filterViaFlatMap(a, (a:Int) => a % 2 == 1)

List.zipWith(a,b, (a:Int, b:Int)=>(a+b))

val ta = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

Tree.size(ta)
Tree.max(ta)
Tree.depth(ta)

