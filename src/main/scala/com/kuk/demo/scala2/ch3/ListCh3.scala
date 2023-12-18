package com.kuk.demo.scala2.ch3

import scala.annotation.tailrec

trait ListCh3[+A]
case object Nil extends ListCh3[Nothing]
case class Cons[+A](head: A, tail: ListCh3[A]) extends ListCh3[A]

object ListCh3{

  def apply[A](as : A*): ListCh3[A] = {
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail: _*))
  }
  extension [A](self: ListCh3[A])
    def take(n: Int): ListCh3[A] = {
      if n <= 0 then Nil
      else if n >= self.size then self
      else self match
        case Nil => self
        case Cons(head, tail) => Cons(head, tail.take(n - 1))
    }

    def drop(n: Int): ListCh3[A] = {
      if n <= 0 then self
      else if n >= self.size then Nil
      else self match
        case Nil => self
        case Cons(head, tail) => Cons(head, tail.drop(n - 1))
    }

    def foldLeft[B](z: B)(f: (B, A) => B): B = self match
      case Nil => z
      case Cons(x, xs) => xs.foldLeft(f(z, x))(f)

    def foldRight[B](z: B)(f: (A, B) => B): B = self match
      case Nil => z
      case Cons(x, xs) => f(x, xs.foldRight(z)(f))

    def toList: List[A] =
      self.foldRight(List.empty)((a, acc) => a +: acc)

    def size: Int = {
      self.foldLeft(0)((acc, _) => acc + 1)
    }

    def filter(f: A => Boolean): ListCh3[A] = {

      @tailrec
      def go(acc: ListCh3[A] = Nil)(list: ListCh3[(A, A => Boolean)]): ListCh3[A] = list match
        case Nil => acc
        case Cons((x, f), xs) if f(x) => go(Cons(x, acc))(xs)
        case Cons(_, xs) => go(acc)(xs)

      go().compose(tupleForEach(f))(self)
    }

  end extension


  def setHead[A](head: A, list: ListCh3[A]): ListCh3[A] = Cons(head, list)

  def add_1(list: ListCh3[Int]): ListCh3[Int] = {
    list.foldRight(Nil: ListCh3[Int])((a, b) => Cons(a + 1, b))
  }

  private def tupleForEach[A, B](f: B)(list: ListCh3[A]): ListCh3[(A, B)] = {
    @tailrec
    def go(acc: ListCh3[(A, B)] = Nil)(list: ListCh3[A]): ListCh3[(A, B)] = list match
      case Nil => acc
      case Cons(x, xs) => go(Cons((x, f), acc))(xs)

    go()(list)
  }

  def map[A, B](list: ListCh3[A])(f: A => B): ListCh3[B] = {

    @tailrec
    def go2( acc: ListCh3[B] = Nil)(list: ListCh3[(A, A => B)]): ListCh3[B] = list match
      case Nil => acc
      case Cons((a, f), xs) => go2( Cons(f(a), acc) )(xs)

//    val it1: List[(A, A => B)] = go(Nil)(list)
//    val it2: List[B] = go2(Nil)(it1)
    go2().compose(tupleForEach(f))(list)
  }



  def reverse[A](list: ListCh3[A]): ListCh3[A] = {
//    @tailrec
//    def go(list: List[A], acc: List[A] = Nil): List[A] = list match
//      case Nil => acc
//      case Cons(x, xs) => go(xs, Cons(x, acc))
//
//    go(list)z
    list.foldLeft(Nil: ListCh3[A])((b, a) => Cons(a, b))
  }

  def concat[A](list1: ListCh3[A], list2: ListCh3[A]): ListCh3[A] = {
    @tailrec
    def go(list1: ListCh3[A], acc: ListCh3[A] = list2) : ListCh3[A] = {
      list1 match
        case Nil => acc
        case Cons(head, tail) => go(tail, Cons(head, acc))
    }

    go(reverse(list1))
  }

  def flatten[A](list: ListCh3[ListCh3[A]]): ListCh3[A] = {
    @tailrec
    def go(list: ListCh3[ListCh3[A]], acc: ListCh3[A] = Nil): ListCh3[A] =
      list match
        case Nil => acc
        case Cons(x, xs) => go(xs, concat(x, acc))

    go(reverse(list))
  }

  def flatMap[A, B](as: ListCh3[A])(f: A => ListCh3[B]): ListCh3[B] = flatten( map(as)(f) )

  def flatMap2[A, B](as: ListCh3[A])(f: A => ListCh3[B]): ListCh3[B] = ???

  def filter_FlatMap[A](list: ListCh3[A])(f: A => Boolean): ListCh3[A] =
    flatMap(list)(a => if f(a) then Cons(a, Nil) else Nil )

  def zip[A, B](listA: ListCh3[A], listB: ListCh3[B]): ListCh3[(A, B)] = {
    def go(listA: ListCh3[A], listB: ListCh3[B], acc: ListCh3[(A, B)] = Nil): ListCh3[(A, B)] =
      (listA, listB) match
        case (Nil, Nil) => acc
        case (_, Nil) => acc
        case (Nil, _) => acc
        case (Cons(xA, xsA), Cons(xB, xsB)) =>
          go(xsA, xsB, Cons((xA, xB), acc))
    reverse(go(listA, listB))
  }

  def zipWith[A, B, C](listA: ListCh3[A], listB: ListCh3[B])(f: (A, B) => C): ListCh3[C] =
    map(zip(listA, listB))( (a, b) => f(a, b) )

  def hasSubsequence[A](list: ListCh3[A], sub: ListCh3[A]): Boolean = {
    @tailrec
    def firstLevelCheck(list: ListCh3[A], sub: ListCh3[A]): Boolean = {
      (list, sub) match
        case (Nil, _) => false
        //first elements matches, go deeper and check the following from sequence
        case (Cons(xL, xsL), Cons(xSub, xsSub)) if xL == xSub =>
          //check next elements in sequence for match
          if startWith(xsL, xsSub) then true
          //mismatch found, continue searching
          else firstLevelCheck(xsL, sub)
        //if x != sub.head, continue searching
        case (Cons(xL, xsL), _) => firstLevelCheck(xsL, sub)
    }

    @tailrec
    def startWith(list: ListCh3[A], sub: ListCh3[A]): Boolean = {
      (list, sub) match
//        case (Nil, Nil) => true
//        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(xL, xsL), Cons(xSub, xsSub)) if xL == xSub => startWith(xsL, xsSub)
        //if xL != xSub
        case (_, _) => false
    }


    firstLevelCheck(list, sub)
  }



  //


  def concatN[A](list: ListCh3[A], n: Int): ListCh3[A] = {

    def loop(n: Int, acc: ListCh3[A] = Nil): ListCh3[A] = {
      if n <= 0 then acc
      else loop(n - 1, concat(list, acc))
    }

    loop(n, Nil)
  }
//14 25
  def takeCircular[A](list: ListCh3[A], n: Int): ListCh3[A] = {
    if n <= 0 then Nil
    else if size(list) <= 0 then Nil
    else{
      val listCopies: Int = n / size(list)
      val modulus = n % size(list)

      concat(concatN(list, listCopies), list.take(modulus))
    }

  }
//7 7*3+4 = 25 => 1-7 7-1 1-7 1,2,3,4
  def takeCircularReverse[A](list: ListCh3[A], n: Int): ListCh3[A] = {
    takeCircular(concat(list, reverse(list)), n)
  }
}


