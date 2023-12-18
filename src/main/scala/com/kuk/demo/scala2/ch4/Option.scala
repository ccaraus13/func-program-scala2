package com.kuk.demo.scala2.ch4

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  def isEmpty: Boolean
  def get: A

}
case class Some[+A](get: A) extends Option[A]{
  override def map[B](f: A => B): Option[B] = Option(f(get))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this

  override def filter(f: A => Boolean): Option[A] = if f(get) then this else None

  override def isEmpty: Boolean = false
}
case object None extends Option[Nothing]{
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: Nothing => Boolean): Option[Nothing] = None

  override def isEmpty: Boolean = true

  override def get: Nothing = throw new NoSuchElementException("try to get element of None!!!")
}

object Option{
  def apply[A](get: A): Option[A] = Some(get)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def map2[A, B, C](optA: Option[A], optB: Option[B])(f: (A, B) => C): Option[C] =
    optA.flatMap(a => optB.map(b => f(a, b)))

  def sequence[A](xs: List[Option[A]]): Option[List[A]] = {
//    xs.foldRight(Option(List.empty[A]))( (optA, optList) => optList.flatMap(list => optA.map(a => a :: list)))
//    xs.foldRight(Option(List.empty[A]))( (optA, optList) => map2(optA, optList)( _ :: _ ))
      traverse(xs)( (opt: Option[A]) => opt)
  }

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] = {
    xs.foldRight( Option(List.empty[B]) ){ (a, optList) =>
      map2(f(a), optList)( _ :: _ )
    }
  }

  def cond[A](a: A)(fn: A => Boolean): Option[A] =
    Some(a).filter(fn)

}
