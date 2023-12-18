import com.kuk.demo.scala2.ch5.{Stream, Empty}

val xs = Stream(1,2,3,4,5,6,7)
val xs2 = Stream(12,13,14,15,16)

/*
def foldRight[A, B](init: B)(f: (A, B) => B): B = this match
  case Nil => init
  case Cons(x, xs) => f(x, xs.foldRight(init)(f))

list = List(1,2,3)
list.foldRight(0)((a, b) => a + b)
1)  f( 1, f( 2, f( 3, 0 ) ) )
*/

//val res0: List[Int] = List(6, 5, 3, 0)
//Stream(1,2,3).scanRight(0)(_ + _).toList
Stream(3).scanRight(0)((a, b) => {println(s"$a + $b"); a + b}).toList

Stream(2, 3).scanRight(0)((a, b) => {println(s"$a + $b"); a + b}).toList

Stream(1, 2, 3).scanRight(0)((a, b) => {println(s"$a + $b"); a + b}).toList

//xs.toList
//
//println("drop")
//xs.drop(0).toList
//xs.drop(1).toList
//xs.drop(8).toList
//xs.getTail.toList
//
println("tails")
//Stream.empty[Int].tails.toList
////Stream(1).tails.map(_.toList).toList
Stream(1,2,3).tails.map(_.toList).toList
Stream(1,2,3).scanRight(Stream.empty[Int])((a, b) => Stream.cons(a, b)).map(_.toList).toList
//Stream(1).tails.toList

//println("startsWith")
//xs.startsWith(xs2)
//
//xs.startsWith(Stream.empty[Int])
//Stream.empty[Int].startsWith(xs)
//
//xs.startsWith(Stream(1))
//xs.startsWith(Stream(1,2,3))
//xs.startsWith(xs)
//
//xs.startsWith(Stream(1,2,13))
//
//xs.zipAll(Stream(1,2,3)).toList
//xs.zipAll(xs).toList

//println("take")
//xs.take(3).toList
//xs.take_unfold(3).toList
////xs.drop(3).toList
//
//println("takeWhile")
//xs.takeWhile(_ < 4).toList
//xs.takeWhileWithFoldRight(_ < 4).toList
//xs.takeWhile_unfold(_ < 4).toList
//
//println("zipWith")
//xs.zipWith(xs2)((i1: Int, i2: Int) => s"($i1+$i2=${i1+i2})").toList
//
//println("zipAll")
//xs.zipAll(xs2).toList

//xs.headOption
//xs.headOption_WithFoldRight

//Empty.headOption
//Empty.headOption_WithFoldRight
//println("map")
//xs.map(_ * 2).toList
//xs.map_unfold(_ * 2).toList
//xs.filter(_ % 2 == 0).toList
//xs2.filter(_ % 2 == 0)

//xs.append(Stream(13,14)).toList

//xs.flatMap(i => Stream.apply(i, i)).toList

//val ones: Stream[Int] = Stream.cons(1, ones)
//val ones: Stream[Int] = Stream.constant(1)
//ones.take(5).toList
//Stream.constant_unfold(1).take(25).toList
//
//ones.map(_ + 1).exists(_ % 2 == 0)
//ones.takeWhile(_ == 1)
////ones.forAll(_ != 1)//StackOverFlow
//
//println("from")
//Stream.from(10).take(7).toList
//Stream.from_unfold(10).take(7).toList
//
//def calcFibs(n: Int): Int = {
//  if n == 0 then 0
//  else if n == 1 then 0 + 1
//  else calcFibs(n - 2) + calcFibs(n - 1)
//}

//val  0, 1, 1, 2, 3, 5, 8, 13, 21
//idx  0, 1, 2, 3, 4, 5, 6, 7, 8
//calcFibs(0) //0
//calcFibs(1) //1
//calcFibs(2) //1
//calcFibs(3) //2
//calcFibs(4) //3
//calcFibs(7) //13
//calcFibs(8) //13

//val fibSeries = Stream.fibs
//val fibSeries_unfold = Stream.fibs_unfold
////fibSeries.take(0)
//fibSeries.take(1).toList
//fibSeries_unfold.take(1).toList
//
//fibSeries.take(2).toList
//fibSeries_unfold.take(2).toList
//
//fibSeries.take(3).toList
//fibSeries_unfold.take(3).toList
//
////fibSeries.take(4).toList
////fibSeries.take(7).toList
////fibSeries.take(8).toList
//fibSeries.take(9).toList
//fibSeries_unfold.take(9).toList