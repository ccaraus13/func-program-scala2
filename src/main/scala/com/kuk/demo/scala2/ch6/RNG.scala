package com.kuk.demo.scala2.ch6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG{
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG{
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (n, r2) = s(rng)
    (f(n), r2)
  }

  def map2[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = sa(rng)
    val (b, r2) = sb(r1)
    (f(a, b), r2)
  }

  def both[A, B](sa: Rand[A], sb: Rand[B]): Rand[(A, B)] =
    map2(sa, sb)( (_, _) )

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, r2) = s(rng)
    val rb: Rand[B] = f(a)

    rb(r2)
  }

  def map_FlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_FlatMap[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(sa)(a => {
      flatMap(sb)(b => unit(f(a, b)))
    })

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThan_flatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan_flatMap(n)
    })

  // 0 to MaxInt
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if n >= 0 then (n, rng2)
    else ( (1 + n) * -1, rng2)
  }

  def int(rng: RNG): (Int, RNG) = rng.nextInt

  //0 to 1(exclusive)
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
//    val divisor = math.pow(10, n.toString.length)
    val divisor = Int.MaxValue.toDouble + 1
    val newDbl = n / divisor
    (newDbl, rng2)
  }

  def double_WithMap: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }



  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, _) = rng.nextInt
    val (dbl, rng2) = double(rng)
    ((i, dbl), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    Option(intDouble(rng)).map((t, r) => (t.swap, r)).get
  }

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (n2, rng2) = double(rng)
    val (n3, rng3) = double(rng2)
    val (n4, rng4) = double(rng3)

    ((n2, n3, n4), rng4)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft( unit(List.empty[A]) )( (rl, ra) => map2(rl, ra)((list, a) => a :: list) )
  }

  def ints(cnt: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to cnt).toList.foldLeft(List.empty[Int] -> rng) { case ((list, rng), _) =>
      val (i, rng2) = rng.nextInt
      (i :: list, rng2)
    }
  }

  def ints_WithSequence(cnt: Int): Rand[List[Int]] =
    sequence( List.fill(cnt)(int) )

  def between(start: Int, endVal: Int): Rand[Int] =
    rng => {
      val vector = (start until endVal).toVector
      val (index, aRNG) = RNG.nonNegativeLessThan(vector.length)(rng)
      (vector(index), aRNG)
    }

  def nextAnyChar: Rand[Char] = rng => {
    val (i, rngB) = between(33, 126 + 1)(rng)
    (i.toChar, rngB)
  }

  def alphanumericChar: Rand[Char] = rng => {
    val possibleValues = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
    val (i, rngB) = between(0, possibleValues.length)(rng)
    (possibleValues(i), rngB)
  }

  def alphaChar: Rand[Char] = rng => {
    val possibleValues = ('a' to 'z') ++ ('A' to 'Z')
    val (i, rngB) = between(0, possibleValues.length)(rng)
    (possibleValues(i), rngB)
  }


  def nextStringFixedSize(length: Int, rnd: => Rand[Char] = nextAnyChar ): Rand[String] = rng => {
    val (list, rngB) = sequence(List.fill(length)(rnd))(rng)

    ( String.valueOf(list.toArray[Char]), rngB )
  }

  def nextString(lengthFrom: Int = 1, lengthTo: Int)(rndRng: => Rand[Char] = nextAnyChar ): Rand[String] =
    flatMap( between( lengthFrom, lengthTo + 1 ) )( length => nextStringFixedSize(length, rndRng)  )

}


