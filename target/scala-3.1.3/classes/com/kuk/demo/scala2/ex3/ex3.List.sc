import com.kuk.demo.scala2.ch3.{Cons, ListCh3, Nil}

val list = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
val list2 = Cons(7, Cons(8, Cons(9, Cons(10, Nil))))
val list3 = Cons(11, Cons(12, Cons(13, Cons(14, Nil))))
val list4 = Cons(11, Cons(12, Cons(13, Cons(14, Cons(15, Nil)))))
val combo = Cons(list, Cons(list2, Cons(list3, Nil)))
ListCh3.add_1(list)

ListCh3.map(list)(_ + 1)
val filter = ListCh3.filter(list)(_ % 2 == 0)
ListCh3.reverse(list)
ListCh3.foldLeft(list, Nil: ListCh3[Int])((b, a) => Cons(a, b))
ListCh3.concat(list, list2)
ListCh3.flatten(combo)

ListCh3.flatMap(list)((i: Int) => Cons(i, Cons(i, Nil)))
val filter_FlatMap = ListCh3.filter_FlatMap(list)(_ % 2 == 0)
ListCh3.zip(list, list2)
ListCh3.zipWith(list, list2)(_ + _)

ListCh3.zip(list, list4)
ListCh3.zipWith(list, list4)(_ + _)

ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(1))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(1,2))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(1,2, 3))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(1,2, 3, 4))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(2, 3))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(3, 4))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(3))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(4))
ListCh3.hasSubsequence(ListCh3(1,2,3,4, 5, 3, 4,5,6, 7), ListCh3(4, 5, 6))

ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(1,2, 3, 4, 5))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(4, 5))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(1,2, 11))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(1,11, 3))
ListCh3.hasSubsequence(ListCh3(1,2,3,4), ListCh3(11,2, 3))
