package com.kuk.demo.scala2.ch4

sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b.map(el => f(a, el)))

}
case class Left[+E, +A](err: E) extends Either[E, Nothing]{
  override def map[B](f: Nothing => B): Either[E, B] = this

  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

}

case class Right[+E, +A](get: A) extends Either[Nothing, A]{
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(get))

  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(get)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this

}

object Either{
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)( (a: Either[E, A]) => a )

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight( Right(List.empty): Either[E, List[B]])( (a, eitList) => f(a).map2(eitList)( _ :: _ ) )
  }

  def cond[E, A](fn: A => Boolean)(a: => A, e: => E ): Either[E, A] =
    if fn(a) then Right(a) else Left(e)
}

//object AccumulatingErrors{
//
//  def leftToRight[E, A](left: Left[E, A]): Right[A, E] = ???
//  def rightToLeft[E, A](right: Right[E, A]): Left[A, E] = ???
//  def oppositeEither[E, A](el: Either[E, A]): Either[A, E] = ???
//
//  def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[List[E], C] = {
//    a.map2(b)(f) match
//      case Left(err) =>
//        val i: Either[C, List[E]] = oppositeEither(a).map2(oppositeEither(b))((e1, e2) => List(e1, e2))
//        oppositeEither[C, List[E]](i)
//      case Right(get) => Right(get)
//  }
//}
