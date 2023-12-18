package com.kuk.demo.scala2.ch7

import com.kuk.demo.scala2.ch6.State
import com.kuk.demo.scala2.ch7

import java.util.concurrent.{Callable, ExecutorService, Executors, Future, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch

type Par[A] = ExecutorService => Future[A]

object Par {

  /**
   * promotes a constant value to parallel computation
   *
   * @param a
   * @tparam A
   * @return
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /**
   * Wraps a constant value into a Future
   *
   * @param get
   * @tparam A
   */
  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  /**
   * take unevaluated A and return a computation that might evaluate in a separate thread
   *
   * @param a
   * @tparam A
   * @return
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * extract result value from Par by actually performing the computation
   *
   * @param a
   * @tparam A
   * @return
   */
  extension[A] (parA: Par[A])
    def run(es: ExecutorService): Future[A] = parA(es)

  /**
   * Marks given Par for concurrent evaluation by run
   *
   * @param a
   * @tparam A
   * @return
   */
  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      override def call(): A = a(es).get()
    })
  }

  def delay[A](fa: => Par[A]): Par[A] =
    (es: ExecutorService) => fa(es)

  /**
   * combines results of two Pars
   * */
  extension[A] (parA: Par[A])
    def map2[B, C](parB: Par[B])(fn: (A, B) => C): Par[C] = (es: ExecutorService) => {
      //      val futureA = parA(es)
      //      val futureB = parB(es)
      //      UnitFuture( fn(futureA.get(), futureB.get()) )

      new Future[C]:
        private val futureA = parA(es)
        private val futureB = parB(es)
        private var cache: Option[C] = None

        override def cancel(mayInterruptIfRunning: Boolean): Boolean =
          futureA.cancel(mayInterruptIfRunning) || futureB.cancel(mayInterruptIfRunning)

        override def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled

        override def isDone: Boolean = cache.isDefined

        override def get(): C = get(Int.MaxValue, TimeUnit.NANOSECONDS)

        override def get(timeout: Long, timeunit: TimeUnit): C = {
          val timeoutNano = TimeUnit.NANOSECONDS.convert(timeout, timeunit)

          val startTime = System.nanoTime()
          val a = futureA.get(timeoutNano, TimeUnit.NANOSECONDS)
          val elapsedTime = System.nanoTime() - startTime

          val b = futureB.get(elapsedTime, TimeUnit.NANOSECONDS)
          val c = fn(a, b)

          cache = Some(c)

          c
        }
    }

  extension[A0] (par0: Par[A0])
    def map3[A1, A2, R](par1: Par[A1], par2: Par[A2])(fn: (A0, A1, A2) => R): Par[R] = {
      val par01 = par0.map2(par1)((a0, a1) => fn.curried(a0)(a1))
      par01.map2(par2)((func, last) => func(last))
    }

  extension[A0] (par0: Par[A0])
    def map4[A1, A2, A3, R](par1: Par[A1], par2: Par[A2], par3: Par[A3])(fn: (A0, A1, A2, A3) => R): Par[R] = {

//      par0.map2(par1)((a0, a1) => {
//        par2.map2(par3)((a2, a3) => fn(a0, a1, a2, a3))
//      })

      val parFirstN = par0.map3(par1, par2)((a0, a1, a2) => fn.curried(a0)(a1)(a2))
      parFirstN.map2(par3)((func, last) => func(last))
    }

  extension[A0] (par0: Par[A0])
    def map5[A1, A2, A3, A4, R](par1: Par[A1], par2: Par[A2], par3: Par[A3], par4: Par[A4])(fn: (A0, A1, A2, A3, A4) => R): Par[R] = {
      val parFirstN = par0.map4(par1, par2, par3)((a0, a1, a2, a3) => fn.curried(a0)(a1)(a2)(a3))
      parFirstN.map2(par4)((func, last) => func(last))
    }

  extension[A] (parA: Par[A])
    def map[B](fn: A => B): Par[B] =
      parA.map2(unit(()))((a, _) => fn(a))

  /**
   * join(unit(a)) == unit(a)
   *
   * @param par
   * @tparam A
   * @return
   */
  def join[A](par: Par[Par[A]]): Par[A] = (es: ExecutorService) => {
    par.run(es).get().run(es)
  }

  extension[A] (parA: Par[A])
    def flatMap[B](fn: A => Par[B]): Par[B] =
      join( parA.map(fn) )


  /**
   * convert a function to one that evaluates asynchronously
   *
   * @param fn
   * @tparam A
   * @tparam B
   * @return
   */
  def asyncF[A, B](fn: A => B): A => Par[B] =
    (a: A) => lazyUnit(fn(a))

  /**
   * sorts list of parallel computation
   *
   * @param parList
   * @return
   */
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
    //    parList.map2(unit(()))((list, _) => list.sorted)
    parList.map(_.sorted)
  }

  /**
   * collects results of parallel computations
   *
   * @param ps
   * @tparam A
   * @return
   */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    sequenceBalanced(ps.toIndexedSeq).map(_.toList)
    //    sequenceSimple(ps)
  }

  /**
   * `sequence` iterative approach
   *
   * @param ps
   * @tparam A
   * @return
   */
  def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List.empty[A]))((parA, parList) => parA.map2(parList)(_ :: _))
  }

  /**
   * `sequence` divide and impera approach
   *
   * @param ps
   * @tparam A
   * @return
   */
  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if ps.length <= 1 then {
      //      lazy val opt = ps.foldRight(unit(List.empty[A]))((parA, list) => parA.map2(list)(_ :: _))
      //      val opt2 = ps.headOption.map(_.map(List(_))).getOrElse(unit(List.empty[A]))
      ps.headOption.map(_.map(IndexedSeq(_)))
        .getOrElse(unit(IndexedSeq.empty[A]))
    } else {
      val (left, right) = ps.splitAt(ps.length / 2)

      //      val parL = fork(sequenceBalanced(left))
      //      val parR = fork(sequenceBalanced(right))

      val parL = sequenceBalanced(left)
      val parR = sequenceBalanced(right)

      parL.map2(parR)(_ ++ _)
    }

  }

  /**
   * apply `fn` in parallel on list elements
   *
   * @param ps
   * @param fn
   * @tparam A
   * @tparam B
   * @return
   */
  def parMap[A, B](ps: List[A])(fn: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(fn))
    sequence(fbs)
  }

  /**
   * filters elements of a list in parallel
   *
   * @param as
   * @param f
   * @tparam A
   * @return
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    parMap(as)(a => if f(a) then List(a) else List.empty[A]).map(_.flatten)
  }

  def parFold[A](ps: IndexedSeq[A])(z: A)(fn: (A, A) => A): Par[A] = {
    if (ps.length <= 1) then Par.unit(ps.headOption.getOrElse(z))
    else {
      val (left, right) = ps.splitAt(ps.length / 2)
      val parL = fork(parFold(left)(z)(fn))
      val parR = fork(parFold(right)(z)(fn))
      parL.map2(parR)(fn)
    }
  }

  def traverse[A, B](ps: List[A])(fn: A => Par[B]): Par[List[B]] = ???

  def foldRight[A, B](ps: List[A])(z: B)(fn: (A, B) => B): Par[B] = {
    foldRightPar(ps)( unit(z) )( (a, parB) => unit(a).map2(parB)(fn) )
  }

  def foldRightPar[A, B](ps: List[A])(z: Par[B])(fn: (A, Par[B]) => Par[B]): Par[B] = {
    if ps.isEmpty then z
    else if ps.length == 1 then fn(ps.head, z)
    else {
      val (left, right) = ps.splitAt(ps.length / 2)
      val parL = fork(foldRightPar(left)(z)(fn))

      fork(foldRightPar(right)(parL)(fn))
    }
  }

  def foldRight2[A, B](ps: List[A])(z: B)(fn: A => B)(gn: (B, B) => B): Par[B] =
    foldRightPar2(ps)(unit(z))(asyncF(fn))(gn)

  def foldRightPar2[A, B](ps: List[A])(z: Par[B])(fn: A => Par[B])(gn: (B, B) => B): Par[B] = {
    if ps.isEmpty then z
    else if ps.length == 1 then fn(ps.head)
    else {
      val (left, right) = ps.splitAt(ps.length / 2)
      val parL: Par[B] = fork(foldRightPar2(left)(z)(fn)(gn))
      val parR: Par[B] = fork(foldRightPar2(right)(z)(fn)(gn))

      parL.map2(parR)((l, r) => gn(l, r))
    }

  }

  extension [A](a: Par[A])
    def equal(a1: Par[A]): Par[Boolean] =
      a.map2(a1)(_ == _)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => {
      if(cond.run(es).get()) then t(es)
      else f(es)
    }

  // (index, A)
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val index = n.run(es).get() % choices.size
    choices(index).run(es)
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cond.map(b => if b then 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(map: Map[K, Par[V]]): Par[V] =
    key.flatMap(k => map(k))
//    es => {
//      val aKey = key.run(es).get()
//      map.get(aKey).map(_.run(es)).get
//    }


  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
//    pa.flatMap(choices)
    es => {
      choices(pa.run(es).get()).run(es)
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if b then t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n.map(i => i % choices.size))(choices.apply)

  def choiceMapViaChooser[K, V](key: Par[K])(map: Map[K, Par[V]]): Par[V] =
    chooser(key)(map.apply)

  def map2ViaFlatMap[A, B, C](parA: Par[A], parB: Par[B])(fn: (A, B) => C): Par[C] =
    parA.flatMap(a => parB.flatMap(b => unit(fn(a, b))))



}



object ParApplication {
  import Par.*

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.length <= 1) then Par.unit( ints.headOption.getOrElse(0) )
    else {
      val (left, right) = ints.splitAt(ints.length / 2)
//      val sumL: Par[Int] = Par.unit(sum(left))
//      val sumR: Par[Int] = Par.unit(sum(right))
//      Par.get(sumL) + Par.get(sumR)
//--
//      Par.map2(Par.fork(sum(left)), Par.fork(sum(right)))(_ + _)
//--
      Par.fork(sum(left)).map2(Par.fork(sum(right)))(_ + _)
    }
  }

  def max(ints: IndexedSeq[Int]): Par[Int] =
    Par.parFold(ints)(0)((l, r) => if l >= r then l else r)

  def countWords(list: List[String]): Par[List[Int]] = {
    val string: String = ???
    val fnSplit: String => List[String] = (s: String) => s.split(" ").toList
    val fnSplitCnt: String => Int = (s: String) => s.split(" ").length
    val words = string.split(" ")

    list.foldRight(0)((s, acc) => s.split(" ").length + acc)
    val parSplitCnt: String => Par[Int] = Par.asyncF((s: String) => s.split(" ").length)
    val parListCnts: Par[List[Int]] = Par.parMap(list)(_.split(" ").length)

    val trvsMap: Par[Int] = Par.traverse(list)(parSplitCnt).map(_.sum)
    val trvsFlatMap: Par[Int] = Par.traverse(list)(parSplitCnt).flatMap(ints => Par.parFold(ints.toIndexedSeq)(0)(_ + _))
    val foldRightRez: Par[Int] = Par.foldRight(list)(0)((s, acc) => acc + s.split(" ").length )
    val foldRightParRez: Par[Int] = Par.foldRightPar(list)(Par.unit(0))((s, acc) => Par.unit(s.split(" ").length).map2(acc)((a, b) => a + b) )
    val foldRightParRez2: Par[Int] = Par.foldRightPar(list)(Par.unit(0))((s, acc) => parSplitCnt(s).map2(acc)((a, b) => a + b) )

//    Par.demon(list)(0)((b, s) => b + fnSplitCnt(s))

    ???

  }

  @main
  def words(args: String*): Unit = {
    val threadPool = Executors.newFixedThreadPool(10)
    val list = List(
      "dasd dasd dasda 312 fsd",//5
      "dasd dasd 312 fsd",//4
      "dasd dasd 312 fsd gfds 7865 asdf",//7
      "dasd", //1
      "" //0
    )
    val strLnFn = (s: String) => if s.trim.isEmpty then 0 else s.trim.split(" ").length
    val fn: String => Par[Int] = Par.asyncF( strLnFn )
    val parRez: Par[Int] = Par.foldRightPar2(list)(Par.unit(0))(fn)(_ + _)
    val parRez2: Par[Int] = Par.foldRight2(list)(0)(strLnFn)(_ + _)
    val parRez3: Par[Int] = Par.foldRight(list)(0)((s, acc) => strLnFn(s) + acc)

    println(parRez.run(threadPool).get())
    println(parRez2.run(threadPool).get())
    println(parRez3.run(threadPool).get())

    threadPool.shutdown()
    while (!threadPool.isTerminated) {}

    println("finish")
  }

  @main
  def test(args: String*): Unit = {
//    import java.util.concurrent.ExecutorService.given

//    val threadPool = Executors.newFixedThreadPool(10)
//    val threadPool = Executors.newFixedThreadPool(3)
    val threadPool = Executors.newCachedThreadPool()
    val list = List(3, 1, 4, 5, 6, 7, 18, 2, 0)

//    println(list.sum)
//    println(Par.unit(10).run(threadPool).get())
//    println(Par.lazyUnit("lazy 13").run(threadPool).get())
//    val parMap2 = Par.lazyUnit(10).map2(Par.lazyUnit("lazy 13"))((i, s) => s"$i > $s")
//    val parMap = Par.lazyUnit(10).map(i => s" < $i >")
//    println(s"parMap2: ${parMap2.run(threadPool).get()}")
//    println(s"parMap2: ${parMap.run(threadPool).get()}")
//
//    val parf = Par.asyncF[Int, Int]( (i: Int) => i * 2)
//    println(s"ParF: ${parf(100).run(threadPool).get()}")
//
//    val parListSorted = Par.sortPar(Par.lazyUnit(list))
//    println(parListSorted.run(threadPool).get())

    println(Par.parFilter(list)(_ > 5).run(threadPool).get())
//    println(sum(list.toIndexedSeq).run(threadPool).get())
    println(Par.parFold(list.toIndexedSeq)(0)(_ + _).run(threadPool).get())

    println(max(list.toIndexedSeq).run(threadPool).get())


    threadPool.shutdown()
    while (!threadPool.isTerminated){}

    println("finish")

  }

  @main
  def testFork(args: String*): Unit = {

    val n = 1
    val threadPool = Executors.newFixedThreadPool(n)

    val nForkPar = (0 to n).foldLeft(Par.unit(1))((par, _) => fork(par)  )

    val a = unit(1)
    val rez = nForkPar.equal(a)(threadPool).get()

    threadPool.shutdown()
    while (!threadPool.isTerminated) {}

    println(s"finish:    rez = $rez")
  }


}
