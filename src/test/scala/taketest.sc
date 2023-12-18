import com.kuk.demo.scala2.ch3.{ListCh3, Nil}

import scala.util.Try

def oupses(i: Int): Int = if i % 2 == 0 then i else throw new Exception("Oups")

Try[Int](oupses(3)).toEither


val list = ListCh3(1,2,3,4,5,6,7)
ListCh3.size(list)
ListCh3.size(Nil)
ListCh3.concat(list, Nil: ListCh3[Int])
ListCh3.concat(list, list)

ListCh3.concatN(list, 0)
ListCh3.concatN(list, 1)
ListCh3.concatN(list, 2)
9 / 7
9 % 7
3 % 7
6 %7

ListCh3.take(list, 3)
ListCh3.take(list, 0)
ListCh3.take(list, 9)
ListCh3.take(list, 7)

ListCh3.takeCircular(list, 3)
ListCh3.takeCircular(list, 0)
ListCh3.takeCircular(Nil, 4)
ListCh3.takeCircular(list, 9)//2
ListCh3.takeCircular(list, 7)//7
ListCh3.takeCircular(list, 7*3+4)//4

ListCh3.takeCircularReverse(list, 3)
ListCh3.takeCircularReverse(list, 0)
ListCh3.takeCircularReverse(Nil, 4)
ListCh3.takeCircularReverse(list, 9)//2
ListCh3.takeCircularReverse(list, 7)//7
ListCh3.takeCircularReverse(list, 7*3+4)//4