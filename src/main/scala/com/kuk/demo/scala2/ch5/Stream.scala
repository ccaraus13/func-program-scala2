package com.kuk.demo.scala2.ch5

import com.kuk.demo.scala2.ch5.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

sealed trait Stream[+A]{
  def headOption: Option[A] = this match
    case Empty => None
    case Cons(head, _) => Some(head())

  def toList: List[A] = {
//    def loop(xs: Stream[A], acc: List[A] = Nil): List[A] = { xs match
//      case Empty => acc
//      case Cons (head, tail) => loop(tail(), head() :: acc)
//    }
//
//    loop(this).reverse
    this match
      case Empty => Nil
      case Cons (head, tail) => head() :: tail().toList
  }

  def isEmpty: Boolean = this match
      case Empty => true
      case _ => false

  def take(n: Int): Stream[A] = {
    this match
      case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
      case Cons(head, _) if n == 1 => cons(head(), empty[A])
      case _ => empty[A]
  }

  def drop(n: Int): Stream[A] = {
//    @tailrec
//    def loop(m: Int = n, xs: Stream[A] = this): Stream[A] = {
//      if m <= 0 then xs
//      else xs match
//        case Empty => xs
//        case Cons(head, tail) => loop(m - 1, tail())
//    }
//
//    loop()
    this match
      case Cons(head, tail) if n > 0 => tail().drop( n - 1 )
      case s => s
  }

  def getTail: Stream[A] = drop(1)

  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(xs: Stream[A] = this): Stream[A] = {
      xs match
        case Cons(head, tail) =>
          val lHead = head()
          if p(lHead) then  cons(lHead, loop(tail()))
          else Empty
        case _ => Empty
    }

    loop()
  }

  def forAll(p: A => Boolean): Boolean = this match
    case Cons(head, tail) => p(head()) && tail().forAll(p)
    case Empty => true

  def foldRight[B](init: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f( h(), t().foldRight(init)(f) )
      case _ => init
    }

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]){ (a, b) =>
      if p(a) then cons(a, b)
      else empty
    }

  def headOption_WithFoldRight: Option[A] = {
    foldRight(None: Option[A]){(a, b) =>
        Option(a)
    }
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B]){(a, b) =>
      cons(f(a), b )
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if f(a) then cons(a, b) else empty)

  def append[A2 >: A](newEl: => Stream[A2]): Stream[A2] =
    foldRight(newEl)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => {
//      f(a).foldRight(b)((a1, a2) => cons(a1, a2))
      f(a).append(b)
    })

  def exists(p: A => Boolean): Boolean =
    this.filter(p).headOption.isDefined

  def map_unfold[B](f: A => B): Stream[B] = {
    val func: Stream[A] => Option[(B, Stream[A])] = (xs: Stream[A]) => {
      xs.headOption.map(a => (f(a), xs.drop(1)) )
    }

    unfold(this)(func)
  }

  def take_unfold(n: Int): Stream[A] = {
    unfold((this, n))( (xs: Stream[A], cnt: Int) =>
      Option.when(cnt > 0)(cnt - 1)
        .flatMap(cnt => xs.headOption.map(a => (a, (xs.drop(1), cnt))) )
    )
  }

  def takeWhile_unfold(p: A => Boolean): Stream[A] = {
    unfold(this)((xs: Stream[A]) => xs.headOption
      .filter(p)
      .map(head => (head, xs.drop(1)) )
    )
  }

  def zipWith[B, C](xs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, xs))((xs1: Stream[A], xs2: Stream[B]) => {
      for{
        a <- xs1.headOption
        b <- xs2.headOption
      } yield ( f(a, b), (xs1.drop(1), xs2.drop(1)) )
    })

  def zipAll[B](xs: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, xs))((xs1: Stream[A], xs2: Stream[B]) => (xs1.headOption, xs2.headOption) match
        case (None, None) => None
        case (head1, head2) => Option((head1, head2), (xs1.getTail, xs2.getTail))
    )
  }

  def startsWith[A2 >: A](xs: Stream[A2]): Boolean = this.zipAll(xs).forAll {
    case (maybeA, maybeB) => maybeB.fold(true)(b => maybeA.contains(b))
  }

  def tails: Stream[Stream[A]] = {
    unfold(this)( (xs: Stream[A]) => xs.headOption.map(_ => (xs, xs.getTail)) )
      .append(Stream(empty[A]))
  }

  //List(6, 5, 3, 0)
  // Stream(1,2,3)
  // f = a + b
  // z = 0
  /*

  def foldRight[B](init: => B)(fRf: (A, => B) => B): B =
    this match {
      case Cons(h, t) => fRf( h(), t().foldRight(init)(fRf) )
      case _ => init
    }

  Stream(1,2,3).foldRight(0 -> Stream(0))((a, xb) => { sRf(a, xb._1) -> cons(sRf(a, xb._1), xb._2 )  } )
   */
  def scanRight[B](init: => B)(sRf: (A, => B) => B): Stream[B] = {
    // O(n^2)
//    unfold(this)( (xs: Stream[A]) => {
//       xs.headOption.map(_ => {
//         val newA = xs.foldRight(z)(f)
//         (newA, xs.getTail)
//       } )
//    } ).append(Stream(z))


//    unfold(this)((xs: Stream[A]) => {
//      xs match
//        case Cons(head, tail) =>
//          lazy val nextFold: B = tail().foldRight(z)(f)
//          val currentVal: B = f(head(), nextFold)
//          Option((currentVal, tail()))
//        case Empty => None
//    }).append(Stream(z))

    foldRight(init -> Stream(init))((a, xb) => {
      lazy val currentValue = sRf(a, xb._1)
      currentValue -> cons(currentValue, xb._2)
    })._2
  }



}
case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]


object Stream{
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](a: A*): Stream[A] = {
    if a.isEmpty then empty
    else cons(a.head, apply(a.tail: _*))
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  //n, n+1, n+2....
  def from(n: => Int): Stream[Int] =
    cons(n, from(n + 1))

  //val  0, 1, 1, 2, 3, 5, 8, 13, 21
  //idx  0, 1, 2, 3, 4, 5, 6, 7, 8
  def fibs: Stream[Int] = {
    // n - the index
//    def calcFibs(n: Int): Int = {
//      if n == 0 then 0
//      else if n == 1 then 0 + 1
//      else calcFibs(n - 2) + calcFibs(n - 1)
//    }

    def calcFibs2(current: Int, next: Int): Stream[Int] = {
      cons(current, calcFibs2(next, current + next))
    }

    calcFibs2(0, 1)

  }

  /**
   * It takes an initial state, and a function for producing
   * both the next state and the next value in the generated stream.
   *
   * @param z
   * @param f
   * @tparam A
   * @tparam S
   * @return
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty[A]
  }

  def constant_unfold[A](a: A): Stream[A] =
    unfold(a)((s: A) => Option((s,s)))

  def from_unfold(n: => Int): Stream[Int] =
    unfold(n)((s: Int) => Option((s, s+1)))

  def fibs_unfold: Stream[Int] = {
    unfold( (0, 1) ) { (current: Int, next: Int) =>
      Option( (current, (next, current + next)) )
    }
  }

  def map_unfold[A, B](xs: Stream[A])(f: A => B): Stream[B] = {
    val func: Stream[A] => Option[(B, Stream[A])] = (xs: Stream[A]) => {
      xs.headOption.map(a => (f(a), xs.drop(1)) )
    }

    unfold(xs)(func)
  }


}