package com.kuk.demo.scala2.ch8

import com.kuk.demo.scala2.ch6.RNG.{Rand, sequence}
import com.kuk.demo.scala2.ch6.{RNG, SimpleRNG, State}
import Prop.*
import Prop.TestCases.*
import Prop.MaxSizes.*
import Prop.Result.*
import com.kuk.demo.scala2.{ch3, ch4, ch5}
import com.kuk.demo.scala2.ch3.{Branch, Leaf, ListCh3, Tree}
import com.kuk.demo.scala2.ch7.Par
import com.kuk.demo.scala2.ch7.Par.*

import java.util.concurrent.{ExecutorService, Executors}


//type Result = Option[(FailedCase, SuccessCount)]

//sealed trait Result{
//  def isFalsified: Boolean
//}
//
//object Passed extends Result{
//  override def isFalsified: Boolean = false
//}
//
//object Proved extends Result{
//  override def isFalsified: Boolean = false
//}
//
//case class Falsified(failedCase: FailedCase, successCount: SuccessCount) extends Result{
//  override def isFalsified: Boolean = true
//}

/**
 * MaxSizes - the maximum size that an element( test case ) may have
 * TestCases - number of elements to generate, that should be examined to consider test passed
 * RNG - the random generator
 * @param run
 */
type Prop = (MaxSizes,TestCases, RNG) => Result

object Prop{
  import Gen.*

  opaque type FailedCase = String
  object FailedCase{
    extension (x: FailedCase) def toString: String = x
    def fromString(x: String): FailedCase = x
  }
  opaque type Label = String
  opaque type SuccessCount = Int

  opaque type TestCases = Int
  object TestCases{
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x
  }

  opaque type MaxSizes = Int
  object MaxSizes {
    extension (x: MaxSizes) def toInt: Int = x

    def fromInt(x: Int): MaxSizes = x
  }

  enum Result{
    case Passed
    case Proved
    case Falsified(failedCase: FailedCase, successCount: SuccessCount)

    def isFalsified: Boolean = this match
      case Passed | Proved => false
      case Falsified(_, _) => true
  }

  extension (self: Prop)
    def &&(p: Prop): Prop = (max: MaxSizes, arg: TestCases, rng: RNG) => {
      self(max, arg, rng) match
        case Passed | Passed => p.tag("and-right")(max, arg, rng)
        case x => x
    }

    def ||(p: Prop): Prop = (max: MaxSizes, arg: TestCases, rng: RNG) => {
      self(max, arg, rng) match
        case Falsified(failedCase, successCount) => p.tag(failedCase).tag("or-right")(max, arg, rng)
        case x => x
    }

    def tag(msg: FailedCase): Prop = (max: MaxSizes, arg: TestCases, rng: RNG) => {
      self(max, arg, rng) match
        case Falsified(failedCase, successCount) => Falsified(s"$msg($failedCase)", successCount)
        case x => x
    }

  end extension

  /**
   *
   * @param p - the property to run
   * @param maxSizes - will be used to generate an element( test case ) up to and including this size
   * @param testCases - number of elements to generate, that should be examined to consider test passed
   * @param rng - the random generator(element generator engine)
   */
  def run(p: Prop,
          maxSizes: MaxSizes = 100,
          testCases: TestCases = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p(maxSizes, testCases, rng) match
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
      case Falsified(msg, successCount) =>
        println(s"! Falsified after $testCases passed tests:\n $msg")

  def forAll[A](g: Gen[A])(fn: A => Boolean): Prop =
    (max, n, rng) =>
      randomStream(g)(rng).zip(LazyList.from(0)).take(n).map {
        case (a, i) => try {
          if fn(a) then Passed else Falsified(a.toString, i)
        } catch
          case e: Exception => Falsified(buildMsg(a, e), i)
    }.find(_.isFalsified).getOrElse(Passed)

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] = {
    LazyList.unfold(rng)(rng => Option(g.run(rng)))
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: Int => Gen[A])(fn: A => Boolean): Prop =
    (maxSize, n, rng) => {
      // 199/100 = (int)1
      val casesPerSize = (n.toInt + maxSize.toInt - 1) / maxSize.toInt
      // 0 to n or maxSize, make one property per size
      val props: LazyList[Prop] = LazyList.from(0).take( (n.toInt min maxSize.toInt) + 1  )
        // g(i) - generator of size i(0, 1, 2 .. n)
        .map(i => forAll(g(i))(fn))

      val prop = props.map[Prop](p =>  (max, n, rng) => {
        //for each prop(that have different size 0 to n or max size), generate 1 (`casesPerSize` value) test case
        // otherwise for each `n` will be created
        p(max, casesPerSize, rng)
      }).toList.reduce(_ && _)

      prop(maxSize, n, rng)
    }

  def forAll[A](g: SGen[A])(fn: A => Boolean): Prop =
    forAll(g(_))(fn)

  def check(p: => Boolean): Prop = (max, n, rng) => {
    lazy val result = p
    if(p) Proved else Falsified("()", 0)
  }

  def checkPar(par: => Par[Boolean]): Prop = {
    import Par.*
    val z: Par[Prop] = par.map(b => check(b))
    ???
  }

  val S: Gen[ExecutorService] = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75D,
    Gen.unit(Executors.newCachedThreadPool()) -> 0.25D
  )

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
  def forAllPar[A](g: Gen[A])(fn: A => Par[Boolean]): Prop =
//    forAll(S.map2(g)( (_, _) )){ case (es, a) => fn(a)(es).get() }
//    forAll(S ** g){ case (es, a) => fn(a)(es).get() }
    forAll(S ** g){ case es ** a => fn(a)(es).get() }



}

type Gen[+A] = State[RNG, A]

/**
 * Knows how to generate values
 * We call the set of values that could be produced by some Gen[A] the domain
 */
object Gen{
  extension[A] (self: Gen[A])
    def map[B](fn: A => B): Gen[B] = {
      self.map(fn)
    }

    def map2[B, C](genC: Gen[B])(fn: (A, B) => C): Gen[C] = ???

    def flatMap[B](fn: A => Gen[B]): Gen[B] =
      self.flatMap(a => fn(a))

    def listOfN(n: Gen[Int]): Gen[List[A]] = {
      //chaining of states are made in sequence: from one state `rng` is pushed to the next one list element
      n.flatMap(i => State.sequence( List.fill(i)(self) ))
    }

    def listOfN(n: Int): Gen[List[A]] = {
      listOfN(Gen.unit(n))
    }

    def streamOf: Gen[ch5.Stream[A]] = State(rng => {
      //      self.map(head => ch5.Stream.cons(head, self.stre))
      lazy val (head, rng2) = self.run(rng)
      val stream = ch5.Stream.cons(head, self.streamOf.run(rng2)._1)
      (stream, rng2)
    })

    def treeOfLeaves(nrLeaves: Int): Gen[Tree[A]] = {
      if nrLeaves <= 1 then self.map(Leaf.apply)
      else if nrLeaves == 2 then {
        val leafGen = self.map(Leaf.apply)
        leafGen.map2(leafGen)(Branch.apply)
      } else {
        val lrSizes = Gen.choose(1, nrLeaves).map(nr => (nr, nrLeaves - nr))
        //          .map(nr => ( nr max (nrLeaves - nr), nr min (nrLeaves - nr) ) )
        //          .map2(Gen.boolean(leftWeight))( (maxMin, maxOnLeft) => if maxOnLeft then maxMin else maxMin.swap)
        val treeGen = lrSizes.flatMap { case (lNr, rNr) => treeOfLeaves(lNr) ** treeOfLeaves(rNr) }
          .map { case (l, r) => Branch(l, r) }

        treeGen
      }
    }

    def treeOfDepth(depth: Int): Gen[Tree[A]] = {
      //balanced tree
      //min depth = 1
      //max Leaves = 2 ^ (depth - 1)
      //min Leaves = depth
      val normDepth = if depth <= 1 then 1 else depth
      val minLeaves = normDepth
      val maxLeaves = math.pow(2, normDepth - 1).toInt
      val nrLeaves = Gen.choose(minLeaves, maxLeaves + 1)

      nrLeaves.flatMap(nr => treeOfLeaves(nr))
    }

    def unsized: SGen[A] = SGen(_ => self)

    def **[B](b: Gen[B]): Gen[(A, B)] =
      self.map2(b)((_, _))

    def zip[B](gb: Gen[B]): Gen[(A, B)] =
      self ** gb

    def withFilter(fn: A => Boolean): Gen[A] = State(rng =>{
      val (a, rng2) = self.run(rng)
      if fn(a) then (a, rng2)
      else withFilter(fn).run(rng2)
    })
  end extension

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    State(RNG.between(start, stopExclusive))
  }

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val state = choose(start, stopExclusive)
    state.map2(state)((_, _))
  }

  def unit[A](a: => A): Gen[A] = State(RNG.unit(a))

  def boolean: Gen[Boolean] = {
    choose(0, 2).map(_ == 0)
  }

  def string: Gen[String] = {
    val genChar: Gen[Char] = State(RNG.alphanumericChar)
    val genInt: Gen[Int] = Gen.choose(0, 10)

    val genList = genChar.listOfN(genInt)
    Gen.map(genList)(list => String.valueOf(list.toArray))
  }

  //    Gen(genA.sample.map(Option.apply))
  def toOption[A](genA: Gen[A]): Gen[ch4.Option[A]] = map(genA)(ch4.Option.apply)
  def fromOption[A](genA: Gen[ch4.Option[A]]): Gen[A] = map(genA)(_.get)


  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] = {
    for {
      bool <- boolean
      a1 <- gen1
      a2 <- gen2
    } yield if bool then a1 else a2
  }

  def weighted[A](gen1: (Gen[A], Double), gen2: (Gen[A], Double)): Gen[A] = {
    val th = gen1._2.abs / (gen1._2.abs + gen2._2.abs)
    State(RNG.double).flatMap(d => if d >= th then gen1._1 else gen2._1)
  }

  def boolean(trueWeight: Double): Gen[Boolean] =
    weighted( (unit(true), trueWeight), (unit(false), 1 - trueWeight) )

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = {
    g.map(i => ( s => i ))
  }


  def genStr2IntFn(g: Gen[Int]): Gen[String => Int] = {
    val fn: Int => String => Int = i => s => {
      s.length * i
    }
    g.map(i => fn(i))
  }

  def genStr3IntFn(g: Gen[Int]): Gen[String => Int] = State(rng => {
    val (seed, rng2) = rng.nextInt
    val fn: String => Int = s => SimpleRNG(seed.toLong ^ s.hashCode.toLong).nextInt._1
    (fn, rng2)
  })


  /**
   *
   * @param g  a generator
   * @param fn  helper function used to generate resulting function, where
   *            - `arg1` is got from current generator,
   *            - `arg2` will be taken from resulting function
   * @tparam A  type of `fn` arguments
   * @tparam B  return type of `fn`
   * @return randomly generated functions
   */
  def genFunc[A, B](g: Gen[A])(fn: (A, A) => B): Gen[A => B] = {
    g.map(i => (j: A) => fn(i, j) )
  }

}

/**
 * Sized Generation
 * @param forSize
 * @tparam A
 */
case class SGen[+A](forSize: Int => Gen[A])
object SGen {
  extension[A] (self: SGen[A])
    def apply(size: Int): Gen[A] = self.forSize(size)

    def map[B](fn: A => B): SGen[B] = SGen(
      size => {
        self(size).map(fn)
      })

    //    def map2[B](fn: A => B): SGen[B]

    def flatMap[B](fn: A => SGen[B]): SGen[B] = SGen(size => self(size).flatMap(a => fn(a)(size)))

    def zip[B](sGen: SGen[B]): SGen[(A, B)] = {
      self.flatMap(a => sGen.map(b => (a, b)))
    }

    def zip[B](gen: Gen[B]): SGen[(A, B)] = {
      val sGen = SGen((_: Int) => gen)
      self.flatMap(a => sGen.map(b => (a, b)))
    }

  end extension

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(size => {
    import Gen.*
    g.listOfN(Gen.unit(size))
  })

  def listOfNonEmpty[A](g: Gen[A]): SGen[List[A]] = SGen(size => {
    import Gen.*
    if size < 1 then g.listOfN(Gen.unit(1))
    else g.listOfN(Gen.unit(size))
  })

  def treeOfDepth[A](g: Gen[A]): SGen[Tree[A]] = SGen(depth => {
    import Gen.treeOfDepth as buildTree
    g.buildTree(depth)
  })

  def treeOfLeaves[A](g: Gen[A]): SGen[Tree[A]] = SGen(nrLeaves => {
    import Gen.treeOfLeaves as buildTree
    g.buildTree(nrLeaves)
  })
}

/*
  Write a sized generator for producing the Tree data type defined in chapter 3,
  and then use this to specify the behavior of the fold function we defined for
  Tree. Can you think of ways to improve the API to make this easier?
* */

object TreeGen{


}

object Play{
  import Gen.*

  @main
  def sequenceEither(args: String*): Unit = {
    //def sequence[A](xs: List[Option[A]]): Option[List[A]]
    val erroMsg = "positive only accepted"

    def toEither(gen: Gen[Int], fn: Int => Boolean): SGen[(List[Int], List[ch4.Either[String, Int]])] = {
      val genOpt = gen.map(i => (i, ch4.Either.cond(fn)(i, erroMsg) ))

      SGen.listOf(genOpt).map(l =>
        l.foldLeft((List.empty[Int], List.empty[ch4.Either[String, Int]])) {
          case ((accI, accO), (i, o)) => (i +: accI, o +: accO)
        })
    }

    val fnPoz = (i: Int) => i >= 0
    val genPozInt = Gen.choose(0, 100)
    val genInt = Gen.choose(-100, 100)

    val sgen = toEither(genInt, fnPoz)
    //    val sgenPoz =  toEither(genPozInt, fnPoz)
    val sgenPoz = toEither(genPozInt, _ => true)

    val propSeqPoz = Prop.forAll(sgen) {
      case (listInt, listOpt) =>
        ch4.Either.sequence(listOpt) == ch4.Either.cond[String, List[Int]]((l: List[Int]) => l.forall(fnPoz)) (listInt, erroMsg)
    } && Prop.forAll(sgenPoz) {
      case (listInt, listOpt) =>
        ch4.Either.sequence(listOpt) == ch4.Right(listInt)
    }

    Prop.run(propSeqPoz)

  }

  @main
  def sequenceOption(args: String*): Unit = {
    //def sequence[A](xs: List[Option[A]]): Option[List[A]]

    def topOpt(gen: Gen[Int], fn: Int => Boolean): SGen[(List[Int], List[ch4.Option[Int]])] = {
      val genOpt = gen.map(i => (i, ch4.Option.cond(i)(fn)))

      SGen.listOf(genOpt).map(l =>
        l.foldLeft( (List.empty[Int], List.empty[ch4.Option[Int]]) ){
          case ((accI, accO), (i, o)) => (i +: accI, o +: accO)
        } )
    }
    val fnPoz = (i: Int) => i >= 0
    val genPozInt = Gen.choose(0, 100)
    val genInt = Gen.choose(-100, 100)

    val sgen = topOpt(genInt, fnPoz)
//    val sgenPoz =  topOpt(genPozInt, fnPoz)
    val sgenPoz =  topOpt(genPozInt, _ => true)

    val propSeqPoz = Prop.forAll(sgen){
      case (listInt, listOpt ) =>
        ch4.Option.sequence(listOpt) == ch4.Option.cond(listInt)(l => l.forall(fnPoz))
    } && Prop.forAll(sgenPoz){
      case (listInt, listOpt ) =>
        ch4.Option.sequence(listOpt) == ch4.Option(listInt)
    }

    Prop.run(propSeqPoz)

  }

  @main
  def aGenTree(s: String*): Unit = {
    import com.kuk.demo.scala2.ch3.Ch3PrintableInstances.*
    import com.kuk.demo.scala2.print.PrintableSyntax.*
    val genTree = Gen.choose(-100, 100).treeOfLeaves(13)//SGen.treeOfLeaves(Gen.string) //treeOF3(5)
    val rng = SimpleRNG(System.currentTimeMillis())
    val tree = genTree.run(rng)._1

//    val genTree = SGen.treeOfLeaves(Gen.string)
//    println(genTree(16).run(rng)._1)
    println(genTree.run(rng)._1)
    tree.print
  }

  @main
  def aSGenTreeLeaves(s: String*): Unit = {
    import com.kuk.demo.scala2.ch3.Ch3PrintableInstances.*
    import com.kuk.demo.scala2.print.PrintableSyntax.*
    val genInt = Gen.choose(-100, 100)
    val genPozInt = Gen.choose(1, 50)
    val treeGen = SGen.treeOfLeaves(genInt)
    val leavesGen = treeGen.map(tree => tree.leaves2)
    val rng = SimpleRNG(System.currentTimeMillis())
    val propLeaves = Prop.forAll(treeGen)(tree => {
      tree.leaves == tree.leaves2
    })

    Prop.run(propLeaves)

  }

  @main
  def aSGenTreeFold(s: String*): Unit = {
    import com.kuk.demo.scala2.ch3.Ch3PrintableInstances.*
    import com.kuk.demo.scala2.print.PrintableSyntax.*
    val genInt = Gen.choose(-100, 100)
    val treeGen = SGen.treeOfLeaves(genInt)
    val propFold = Prop.forAll(treeGen){ tree =>
      tree.fold[Int]( (v: Int) => v, _ + _) == tree.leaves2.sum
    }

    Prop.run(propFold)
  }

  @main
  def listTake(args: String*): Unit = {
    val intGen = Gen.choose(-100, 100)
    val pozGen = Gen.choose(0, 100)
    val listGen: SGen[ListCh3[Int]] = SGen.listOf(intGen)
      .map(l => ch3.ListCh3(l.toArray*))
    val pairs = listGen.zip(pozGen)
    val propTake = Prop.forAll(pairs){case (ls, n) =>
      val l1 = ls.take(n).toList
      val lExpected = ls.toList.take(n)

      lExpected == l1
    } && Prop.forAll(pairs){case (ls, n) => ls.take(n).size == (n min ls.size) }

    Prop.run(propTake)
  }

  @main
  def listDrop(args: String*): Unit = {
    val intGen = Gen.choose(-100, 100)
    val pozGen = Gen.choose(0, 100)
    val listGen: SGen[ListCh3[Int]] = SGen.listOf(intGen)
      .map(l => ch3.ListCh3(l.toArray *))
    val pairs = listGen.zip(pozGen)
    val propTake = Prop.forAll(pairs) { case (ls, n) =>
      val l1 = ls.drop(n).toList
      val lExpected = ls.toList.drop(n)

      lExpected == l1
    } && Prop.forAll(pairs) { case (ls, n) => ls.drop(n).size == (n min ls.size) }

    Prop.run(propTake)
  }

  @main
  def listFilter(args: String*): Unit = {
    val intGen = Gen.choose(-100, 100)
    val listGen: SGen[ListCh3[Int]] = SGen.listOf(intGen)
      .map(l => ch3.ListCh3(l.toArray *))
    val filterGen: Gen[Int => Boolean] = Gen.genFunc(intGen)((i, j) => j > i)
    val pairs = listGen.zip(filterGen)
    val prop = Prop.forAll(pairs) { case (ls, fn) =>
//      println(ls.toList)
//      println(ls.toList.filter(fn))
//      println("=======================")
      ls.filter(fn).toList == ls.toList.filter(fn)
    }

    Prop.run(prop)
  }

  @main
  def streamTake(args: String*): Unit = {
    val pozGen = Gen.choose(0, 100)
    val streamGen = Gen.choose(-100, 100).streamOf
    val pairs = streamGen.zip(pozGen)
    val propTake = Prop.forAll(pairs) { case (ls, n) => ls.take(n).toList.size == n }

    Prop.run(propTake)
  }

  @main
  def streamDrop(args: String*): Unit = {
    val pozGen = Gen.choose(0, 100)
    val streamGen = Gen.choose(-100, 100).streamOf
    val pairs = streamGen.zip(pozGen)
    val propTake = Prop.forAll(pairs) { case (ls, n) => ls.take(n).drop(n).isEmpty }

    Prop.run(propTake)
  }

  @main
  def streamFilter(args: String*): Unit = {
    val intGen = Gen.choose(-100, 100)
    val funcGen = Gen.genFunc(intGen)((i, j) => j < i)
    val streamGen = Gen.choose(-100, 100).streamOf
    val pairs = streamGen.zip(funcGen)
    val prop = Prop.forAll(pairs) { case (ls, fn) =>
      ls.filter(fn).forAll(fn)
    }

    Prop.run(prop)
  }

  @main
  def streamTakeWhile(args: String*): Unit = {
    val intGen = Gen.choose(-100, 100)
    val funcGen = Gen.genFunc(intGen)((i, j) => j < i)
    val streamGen = Gen.choose(-100, 100).streamOf
    val pairs = streamGen.zip(funcGen)
    val prop = Prop.forAll(pairs) { case (ls, fn) =>
      ls.takeWhile(fn).forAll(fn)
    }

    Prop.run(prop)
  }

  @main
  def streamUnfold(args: String*): Unit = {
    val intGen = Gen.choose(-100, 100)
    // S => Option[(A, S)]
    // Gen[A => B]
    val op: (Int, Int) => Option[(Int, Int)] = { (i, j) =>
      if j < i then Some((math.pow(2, j).toInt, j + 1)) else None
    }
    val fnConst: Int => Option[(Int, Int)] = op.curried(10)
    val funcGen = Gen.genFunc(intGen)(op)

    val prop = Prop.forAll(intGen) { i =>
      ch5.Stream.unfold(i)(fnConst).forAll(_ <= math.pow(2, 10).toInt)
    } && Prop.forAll(funcGen) { fn =>
        (ch5.Stream.unfold(0)(fn).headOption, fn(0)) match
          case (Some(v1), Some(v2)) => v1 == v2._1
          case (None, None) => true
          case _ => false
  }

    Prop.run(prop)
  }

  @main
  def playGenFunc(args: String*): Unit = {
    val genInt = Gen.choose(-10, 10)
    val genStr2Int = genStr2IntFn(genInt)
    val fnStr: String => Int = s => s.length
//    val fn: Int => String => Int = i => s => {
//      s.length * i
//    }
    val opInt: (Int, Int) => Int = (i1,i2) => i1*i2
    val fn: Int => (String => Int) = i => s => {
      opInt(i, fnStr(s))
    }

    val genStr3Int = genStr3IntFn(genInt)
//    val genStr4Int = genStr4IntFn(genInt)(opInt)(fnStr)
//    val genFoldXStrInt: Gen[String => Int] = genFoldX(genInt)(fnStr)( (fnStr2Int, i) => fnStr2Int.andThen(j => j * i))
    val rng = SimpleRNG(33L)
//    val rezInt = genStr2Int.run(rng)._1("astring")
//    val rezInt = genStr3Int.run(rng)._1("astring")
//    val rezInt = genStr4Int.run(rng)._1("astring")
    val rezInt = genStr3Int.run(rng)._1("astring")

    println(rezInt)

    //=================
    val genInt10to10: Gen[Int] = Gen.choose(-10, 10)
    val genListOfInt: Gen[List[Int]] = genInt10to10.listOfN(10)

    val genFuncs = Gen.genFunc(genInt10to10)((i, j) => { println(s"$j > $i = ${i+j}"); j > i })
    val genPairs: Gen[ (List[Int], Int => Boolean) ] = genListOfInt.zip(genFuncs)

//    val genFuncsPoz2 = Gen.genFoldX(genInt10to10)( (i: Int) => { println(s"$i > 0"); i > 0 })( (fn, j) => fn.compose(_ => j) )
//    val genPairsPoz: Gen[ (List[Int], Int => Boolean) ] = genListOfInt.zip(genFuncsPoz2)


    val listPred: Gen[List[Int => Boolean]] =  Gen.genFunc(genInt10to10)((i1, i2) => i1 > i2).listOfN(10)
    val geniFn = genListOfInt.map(list => listPred.map(lFn => list.zip(lFn)))



//    val genP: Gen[ (List[Int], Int => Boolean) ] = ???
    val prop = Prop.forAll(genPairs)(ns => {
      val fn = ns._2
      println(s"init:  " + ns._1)
      val list = ns._1.takeWhile(fn)
      println("after: " + list)
      println("================================")
      list.forall(fn)
    })
    Prop.run(prop)
//    Gen.genFold2(genIntPoz)( (i: Boolean) =>  0)((j, i) => fn.andThen(j => i))
//    val takeWhileProp = Prop.fo
  }

  @main
  def main(args: String*): Unit = {
//    val genPoz = Gen.choosePair(0, 24).listOfN(Gen.unit(10))
//    val genNeg = Gen.choosePair(-25, -1).listOfN(Gen.unit(10))
////    val genStrings1 = Gen(State( RNG.nextString(3,10)(RNG.alphanumericChar)(_) )).listOfN(10)
//    val genStrings2 = Gen.string.listOfN(Gen.unit(10))
//    val listOfList = genStrings2.listOfN(Gen.unit(4))
////    val gen = Gen.choose(-10, 24)
//    val rng = SimpleRNG(45L)
//    println(genPoz.run(rng)._1)
//    println(genStrings2.run(rng)._1)
//    println(listOfList.run(rng)._1)
//
//
//
//    val genChar: Gen[Char] = State(RNG.alphanumericChar)
//    println(genChar.listOfN(Gen.unit(10)).run(rng))
//
//    val union = Gen.union(genPoz, genNeg)
//    println(union.run(rng))

    val rng = SimpleRNG(System.currentTimeMillis())
    val smallInt = Gen.choose(-10, 10)




//    val rez = smallInt.run(rng)._1
//    val rez = smallInt.listOfN(Gen.unit(5)).run(rng)._1
//    val rez = SGen.listOf(smallInt)(5).run(rng)._1
    val sgen = SGen.listOfNonEmpty(smallInt)
    val prop = Prop.forAll(smallInt){
      i => i.abs <= 10

    }
    val zz = LazyList.from(1).take( 10 + 1  ).map(i => sgen(i))

//    val rez = prop.run(10, 10, rng)
//    val rez = zz.toList.map(g => g.run(rng)._1)
    val rez = sgen(0).run(rng)._1
    println(rez)
  }

  @main
  def testDemo(args: String*): Unit = {
    import Prop.*
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(SGen.listOfNonEmpty(smallInt)){ ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)

  }

  @main
  def testSorted(args: String*): Unit = {
    import Prop.*

    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(SGen.listOfNonEmpty(smallInt)) { ns =>
      val list = ns.sorted
      val z = list.headOption.map(i => (i, true))
      list.tail.foldLeft(z) { case (acc, current) =>
        acc.map((i, bool) => (current, current >= i && bool))
      }.forall(_._2)
    }

    Prop.run(maxProp)
  }

  @main
  def testCheck(args: String*): Unit = {
    import Par.*
    val es: ExecutorService = Executors.newSingleThreadExecutor()
    val par: Par[Boolean] = Par.map(Par.unit(1))(_ + 1).equal(Par.unit(2))

    val prop = Prop.check(Par.map(Par.unit(1))(_ + 1).equal(Par.unit(2))(es).get() )
//    {
//      val p1 = Par.map(Par.unit(1))(_ + 1)
//      val p2 = Par.unit(2)
////      p1(es).get() == p2(es).get()
//      p1.equal(p2)(es)
//    }

//    Prop.run(prop)

    def sumTillN(n: Int): Int = (0 to n).sum
    def sumTillNPar(n: Int): Par[Int] =
      (0 to n).foldLeft(Par.unit(0))((par, i) => par.map2(Par.unit(i))(_ + _)  )

    val pint = Gen.choose(0, 100).map(Par.unit)
    val prop4 = Prop.forAllPar(pint)(n => n.map(i => i).equal(n) )

    //val nForkPar = (0 to n).foldLeft(Par.unit(1))((par, _) => fork(par)  )
    val multiNestedPars = Gen.choose(0, 1000).map(sumTillNPar)
    val propNestd = Prop.forAllPar(multiNestedPars)(n => n.map(i => i).equal(n) )

//    Prop.run(propNestd)
//    val forkProp = Gen.choose(0, 100).map(i => Par.fork(Par.unit(i)))
    val propFork = Prop.forAllPar(multiNestedPars)(n => Par.fork(n).equal( n ))
    Prop.run(propFork)



//    fork(x) == x
//    Prop.check{
//      Par.fork(sumTillNPar(10)).equal(Par.unit(1 + 2 +3 +4+5+6+7+8+9+10))
//    }
//
//    Par.fork(multiNestedPars)
  }

  @main
  def hoFuncProp(str: String*): Unit = {

    val list: List[Int] = ???
    val fn: Int => Boolean = ???

    list.takeWhile(fn).forall(fn) == true
    list.dropWhile(fn).forall(fn) == false

    list.takeWhile(fn).dropWhile(fn).isEmpty == true

    (list.takeWhile(fn).headOption, list.find(fn)) match
      case ( Some(v1), Some(v2) ) => v1 == v2
      case (None, None) => true
      case (_,_) => false
  }


}
