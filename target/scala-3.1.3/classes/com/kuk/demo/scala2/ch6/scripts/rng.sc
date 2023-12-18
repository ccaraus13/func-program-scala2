import com.kuk.demo.scala2.ch6.SimpleRNG
import com.kuk.demo.scala2.ch6.RNG
import com.kuk.demo.scala2.ch6.RNG.between

//Char.MinValue.toLong
//Char.MaxValue
//(Char.MaxValue.toInt -1000).toChar
//String.valueOf(List(40.toChar, 41.toChar, 42.toChar).toArray[Char])
//'a'.toInt
val rng = SimpleRNG(42)

//RNG.between(33, 126 + 1)(rng)

RNG.nextAnyChar(rng)
RNG.alphanumericChar(rng)
//
RNG.nextStringFixedSize(10, RNG.alphanumericChar )(rng)
RNG.nextString(4, 10)( RNG.alphanumericChar )(rng)

//val (n1, rng2) = rng.nextInt
//
//val (n2, rng3) = rng2.nextInt
//
//rng.nextInt
//rng2.nextInt
//
//RNG.nonNegativeInt(SimpleRNG(228577659274450L))
//SimpleRNG(63684315534556L).nextInt
//RNG.double(SimpleRNG(2435333034912L))
//RNG.double_WithMap(SimpleRNG(2435333034912L))
//
//
//RNG.ints(10)(rng)
//RNG.ints_WithSequence(10)(rng)
//
//10 % 7
//
//RNG.nonNegativeLessThan(23)(rng)
//RNG.nonNegativeLessThan_flatMap(23)(rng)
//
//RNG.map(RNG.int)(i => (i, s"rand in is $i"))(rng)
//RNG.map_FlatMap(RNG.int)(i => (i, s"rand in is $i"))(rng)
//
//RNG.map2(RNG.int, RNG.double)((i, d) => s"[i,d] > [$i, $d]")(rng)
//RNG.map2_FlatMap(RNG.int, RNG.double)((i, d) => s"[i,d] > [$i, $d]")(rng)