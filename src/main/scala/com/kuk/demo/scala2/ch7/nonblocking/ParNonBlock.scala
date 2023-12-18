package com.kuk.demo.scala2.ch7.nonblocking

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference
import scala.util.{Failure, Success, Try}



sealed trait Future[+A]{
//  private [nonblocking] def apply(cb: A => Unit, onError: Throwable => Unit = ex => ()): Unit
  private [nonblocking] def apply(cb: A => Unit): Unit
}

type Par[+A] = ExecutorService => Future[A]

object Par{
  /**
   * extract result value from Par by actually performing the computation
   *
   * @param a
   * @tparam A
   * @return
   */
  extension[A] (parA: Par[A])
    def run(es: ExecutorService): A = {
      val ref = new AtomicReference[A]()
      val latch = new CountDownLatch(1)

      parA(es){ (a: A) => ref.set(a); latch.countDown(); }
      latch.await()
      ref.get()
    }
/*
    def run2(es: ExecutorService): Try[A] = {
      val ref = new AtomicReference[Try[A]]()
      val latch = new CountDownLatch(1)

      //TODO try to get ride of Future apply onError param: since requires to be populated everywhere, some how catch exception in current `run` method
      //error is thrown in different thread(in `eval#es.submit(r)` ) that why cannot be caught
      // mapping parA to Par[Try[A]] does not solve the issue, evaluation of (a:A) is made in different thread, see `eval`

//working example
      parA(es)(
        a => {
          show("Callback"); ref.set(Success(a)); latch.countDown()
        },
        ex => {
          show(s"Callback Error: $ex"); ref.set(Failure(ex)); latch.countDown();
        }
      )

      latch.await()
      ref.get()
    }
*/
    def map2[B, C](parB: Par[B])(fn: (A, B) => C): Par[C] =
      es => new Future[C]:
        override private[nonblocking] def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](es) ({
            case Left(a) => br match
              case Some(b) => eval(es)(cb(fn(a, b)))
              case None => ar = Some(a)

            case Right(b) => ar match
              case Some(a) => eval(es)(cb(fn(a, b)))
              case None => br = Some(b)
          })

          parA(es)(a => {combiner ! Left(a)})
          parB(es)(b => {combiner ! Right(b)})
        }

    def map[B](fn: A => B): Par[B] =
      parA.map2(unit(()))((a, _) => fn(a))

  def unit[A](a: A): Par[A] = (es: ExecutorService) =>
    new Future[A]:
      override private[nonblocking] def apply(cb: A => Unit): Unit =
        cb(a)

  def delay[A](par: => Par[A]): Par[A] = (es: ExecutorService) => par(es)

  def fork[A](par: => Par[A]): Par[A] = (es: ExecutorService) =>
    new Future[A]:
      override private[nonblocking] def apply(cb: A => Unit): Unit =
        eval(es)( par(es)( a => cb(a) ) )

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * A helper function to
   * evaluate an action
   * asynchronously using some
   * ExecutorService.
   *
   * @param es
   * @param r
   */
  def eval(es: ExecutorService)(r: => Unit, onError: Throwable => Unit = ex => () ): Unit =
    es.submit(new Callable[Unit] {
      override def call(): Unit =
        try r
        catch
          case ex: Throwable => onError(ex)

    })

  def show(v: Any): Unit =
    val currentThreadName =  Thread.currentThread().getName
    println(s"[$currentThreadName] $v")
}

object ParNonBlock {
  import Par.*

  @main
  def test(args: String*): Unit = {
    val es = Executors.newFixedThreadPool(1)

    def oupses(i: => Int): Int = {
      show("run `oupses`")
      if i % 2 == 0 then i
      else throw new Exception("Oups")
    }

//    println(fork(unit(Try(oupses(3)))).run(es))
//    println(fork(unit(oupses(3))).map( a => Try(a )).run(es))
//    val parVal2 = lazyUnit(oupses(2))
////    println(parVal2)
//    val parValOups3 = lazyUnit(oupses(3))
//    println(parValOups3.run2(es))
//    val result = parVal2.map2(parValOups3)(_ + _).run2(es)

//    val result = Par.fork(Par.unit(oupses(3))).run(es)
//    val result = fork(unit(oupses(3))).map(Try(_)).run2(es)
//    val result = fork(unit(oupses(3))).run2(es)
//    val result = unit(oupses(3)).run(es)
//    val result = lazyUnit(oupses(3)).run2(es)
//    val result = unit(oupses(3)).run2(es)
//    val result: Int =  Par.fork(Par.unit(1)).run(es)
//    val result = unit(oupses(3)).map(Try(_)).run(es)

//    println(s"rez = $result")

    es.shutdown()
    while (!es.isTerminated) {}

    println(s"finish    ")
  }

}
