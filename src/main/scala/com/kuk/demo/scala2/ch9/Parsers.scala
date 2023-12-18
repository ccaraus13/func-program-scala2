package com.kuk.demo.scala2.ch9

import com.kuk.demo.scala2.ch8.Prop
import com.kuk.demo.scala2.ch8.Gen
import com.kuk.demo.scala2.ch9.JSON.{JArray, JBool, JNumber, JObject, JString}

import scala.language.implicitConversions
import scala.util.matching.Regex

trait ParserErrorType{}
//"Expected one or more 'a'"
case class Location(input: String, position: Int)
type ErrorMessage = String
case class ParserErrors(list: List[(Location, ErrorMessage)] = Nil){
  def push(l: Location, errorMessage: ErrorMessage): ParserErrors = ParserErrors((l, errorMessage) +: list)
}

trait Parsers[ParserError, Parser[+_]]{ self =>

  object Laws{
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(str => run(p1)(str) == run(p2)(str))

    def mapLaw[A](p1: Parser[A])(in: Gen[String]): Prop =
      equal(p1, map(p1)(a => a))(in)

    //flatMap(fn(x))(fn) => fn(x)
    def flatMapLaw[A](unit: A => Parser[A])(in: Gen[A]): Prop =
      Prop.forAll(in){x =>
        val p1 = flatMap(unit(x))(unit)
        val p2 = unit(x)
        run(p1)(x.toString) == run(p2)(x.toString)
      }

    //flatMap(p)(unit) == p
    def flatMapLawString(in: Gen[String]): Prop =
      flatMapLaw(string)(in)

    def flatMapLawChar(in: Gen[Char]): Prop =
      flatMapLaw(char)(in)

    def charLaw(in: Gen[Char]): Prop =
      Prop.forAll(in)(c => run(char(c))(c.toString) == Right(c))

    def stringLaw(in: Gen[String]): Prop =
      Prop.forAll(in)(str => run(string(str))(str) == Right(str))

    def orLaw[A](in: Gen[String]): Prop = {
      import Prop.&&
      import self.string as parserOfString
      import com.kuk.demo.scala2.ch8.Gen.{flatMap, map, withFilter}

      val parseGen = for{
        s <- in
        i <- Gen.choose(0, s.length)
        (str1, str2) <- Gen.unit(s.splitAt(i))
      } yield (str1 + str2, parserOfString(str1), parserOfString(str2))

      //TODO do we need it??
      val prop = Prop.forAll(parseGen){ case(str, p11, p22) =>
        val pOr1 = p11 or p22
        val pOr2 = p22 or p11
        run(p11)(str) == run(pOr1)(str) &&
        run(p11)(str) == run(pOr2)(str) &&
          run(p22)(str) == run(pOr1)(str) &&
          run(p22)(str) == run(pOr2)(str)
      }

      prop

    }


//    /*
//    * run(string("abra") and string("cadabra"))("abracadabra") == Right("abracadabra")
//    run(string("abra") and string("cadabra"))("cadabraabra") == Right("cadabraabra") ???
//    * */
//    def andLaw[A](in: Gen[(String, String)]): Prop = {
//      import Prop.&&
//      import self.string as parserOfString
//      import com.kuk.demo.scala2.ch8.Gen.{flatMap, map, withFilter}
//
//      //TODO do we need it??
//      val prop = Prop.forAll(in) { case (str1, str2) =>
//        val str = str1 + str2
//        val p11 = parserOfString(str1)
//        val p22 = parserOfString(str2)
//        val pAnd1 = p11 and p22
//        val pAndReverse = p22 and p11
//        val aParser = parserOfString(str)
//
//        val reverseProp = (str.isEmpty && run(aParser)(str) == run(pAndReverse)(str)) ||
//          ( str.nonEmpty && run(aParser)(str) != run(pAndReverse)(str) )
//
//        run(aParser)(str) == run(pAnd1)(str) &&
//          reverseProp
//      }
//
//      prop
//    }

    //run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    def listOfNLaw(in: Gen[(Int, String)]): Prop = {
      Prop.forAll(in)( (m, str) => {
        val n = m max 0
        val parser: Parser[String] = string(str)
        run(parser.listOfN(n))(str * n) == Right(List.fill(n)(str)) &&
        run(parser.listOfN(n))(str * (n + 1)) == Right(List.fill(n + 1)(str))
      })
    }

//    def nOrMoreLaw(in: Gen[(Int, String)]): Prop = {
//      Prop.forAll(in)((m, str) => {
//        val n = m max 0
//        val parser: Parser[String] = string(str)
//
//        run(manyN(n, parser))(str * n) == Right(List.fill(n)(str)) &&
//        run(manyN(n, parser))(str * (n + 1)) == Right(List.fill(n + 1)(str))
//      })
//    }

    //run(nOrMoreFollowed(2, 'a')(3, 'b'))("aaabbbb") == Right((3,4))
//    def nOrMoreFollowedLaw(in: Gen[( (Int, String), (Int, String) )]): Prop =
//
//      Prop.forAll(in){ case ((m1, str1), (m2, str2)) =>
//        val n1 = m1 max 0
//        val n2 = m2 max 0
//
//        run(nOrMoreFollowed(m1, str1)(m2, str2))( (str1 * m1) + (str2 * m2) ) == Right(n1, n2)
//      }

    def succeedLaw[A](in: Gen[(A, String)]): Prop =
      Prop.forAll(in)( (a, str) => run(succeed(a))(str) == Right(a))

    /**
     *
     * @param p1
     * @param p2
     * @return
     */
    def prodLaw[A](p1: Parser[String], p2: Parser[String])(in: Gen[(A, String)]): Prop = {
      //unit(a) ** unit(a) == unit((a,a))
     //run(unit(a) ** unit(a))(s) == Right((a, a)) ->
//      p1.product(p2)
      Prop.forAll(in) { (a, str) =>
        run(succeed(a) ** succeed(a))(str) == run(succeed((a, a)))(str)
      }
    }
  }

  extension [A](parser: Parser[A])
    /**
     * map(p)(a => a) == p
     * run(map(p(a))(unit))(a.toString) == run(p(a))
     *
     * @param p
     * @param fn
     * @tparam A
     * @tparam B
     * @return
     */
    def map[B](fn: A => B): Parser[B] =
      parser.flatMap(a => succeed(fn(a)))

    /**
     * Laws
     * map2(p1, p2)(fnAB) = flatMap(p1)(a1 => map(p2)(a2 => fnAB(a1,a2))
     * map2(p1, p2)(fnAB) == map(p1)(a => a) map(p2)(b => b)
     * map2(p, p)(fn) -> fn( map(p)(a => a), map(p)(b => b))
     * map2(p, p)((a, a) => (a, a)) ==? map(p)(a1 => map(p)(a2 => a2))
     *
     * @param pa
     * @param pb
     * @param fn
     * @tparam A
     * @tparam B
     * @tparam C
     * @return
     */
    def map2[B, C](pb: => Parser[B])(fn: (A, B) => C): Parser[C] =
//      parser.product(pb).map(fn(_, _))
//      parser.product(pb).map(fn.tupled)
      parser.flatMap(a => pb.map(b => fn(a, b)) )

    /**
     * Laws:
     * run(zeroOrMore('a'))("aa") == Right(2)
     * run(zeroOrMore('a'))("aaGa") == Right(3)
     * run(zeroOrMore('a'))("bbbaaGa") == Right(3)
     * run(zeroOrMore('a'))("") == Right(0)
     * run(zeroOrMore('a'))("b123") == Right(0)
     *
     * @return
     */
    def many: Parser[List[A]] = {
//      def delay[B](p: => Parser[B]): Parser[B] = p
//      parser.map2(delay(many))(_ :: _) or succeed(List.empty[A])
      parser.map2(many)(_ :: _) or succeed(List.empty[A])
    }

    /**
     * Laws:
     * run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
     * run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
     * run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
     *
     * @param n
     * @param parser
     * @tparam A
     * @return
     */
    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(List())
      else parser.map2(listOfN(n - 1))(_ :: _)

    /**
     * Laws:
     * run(oneOrMore('a'))("aa") == Right(2)
     * run(oneOrMore('a'))("") == Left(0)
     * run(oneOrMore('a'))("b123") == Left(0)
     *
     * @return
     *
     * many1(p) is just p followed by many(p)
     */
    def many1: Parser[List[A]] = parser.map2(parser.many)((a, list) => a +: list)

    /**
     * Laws:
     * run(nOrMore(2, 'a'))("aa") == Right(2)
     * run(nOrMore(2, 'a'))("aaaa") == Right(4)
     * run(nOrMore(3, 'a'))("aa") == Left(2)
     * run(nOrMore(2, 'a'))("") == Left(0)
     * run(nOrMore(2, 'a'))("b123") == Left(0)
     *
     * @return
     */
    def manyN(n: Int = 0): Parser[List[A]]

    /**
     * return the portion of the input
     * string examined by the parser if successful
     *
     * @return
     */
    def slice: Parser[String] = parser.map(_.toString)

    /**
     * def unit[A](a: A): Parser[A]
     *
     * flatMap(p)(fn) ==
     * flatMap(p)(unit) == p
     * flatMap[A](p)(_ => p) == p
     * flatMap(fn(x))(fn) => fn(x)
     *
     * @param p
     * @param fn
     * @tparam A
     * @tparam B
     * @return
     */
    def flatMap[B](fn: A => Parser[B]): Parser[B]

    def option: Parser[Option[A]] =
      parser.map(Option.apply)

    def *>[B](pb: Parser[B]): Parser[B] =
      parser.slice.map2(pb)((_, b) => b)

    def <*[B](pb: Parser[B]): Parser[A] =
      parser.map2(pb.slice)((a, _) => a)

    def attempt: Parser[A]

    def token: Parser[A] =
      parser.attempt <* whitespaces

    /**
     * Zero or more repetitions on `parser` followed by separator
     * @param separator
     * @return
     */
    def sep(separator: Parser[Any]): Parser[List[A]] =
      parser.sep1(separator) | succeed(Nil)

    /**
     * One or more repetitions on `parser` followed by separator
     * @param separator
     * @return
     */
    def sep1(separator: Parser[Any]): Parser[List[A]] =
      parser.map2( (separator *> parser).many )(_ :: _)

//    def withFilter(a: A => Boolean): Parser[A]

  end extension
//=============================//

  def run[A](parser: Parser[A])(input: String): Either[ParserError, A]

  /**
   * Law run(char(c))(c.toString) == Right(c)
   *
   * @param c
   * @return
   */
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  /**
   * Always succeed with value `a`, `unit`
   * @param a
   * @return
   */
  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def failed[A](msg: String): Parser[Nothing]

  def digits: Parser[String] =
    regex("[0-9]*".r)

  def whitespaces: Parser[String] =
    regex("[\\n\\r\\s]*".r)

  def escapedQuotes: Parser[String] = {
    regex("\"(.+)\"".r).token
  }

  def regex(r: Regex): Parser[String]

  def doubleString: Parser[String] =
    regex("[+-]?([0-9]*\\.)?[0-9]+([eE][+-]?[0-9]+)?".r)

  def double: Parser[Double] = doubleString.map(_.toDouble)
  def boolean: Parser[Boolean] = (string("true") | string("false")).map(v => v.toBoolean)

  /**
   * Laws:
   * run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
   * run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")
   *
   * @param s1
   * @param s2
   * @tparam A
   * @return
   */
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  /**
   * ????
   * Laws:
   * run(string("abra") and string("cadabra"))("abracadabra") == Right("abracadabra")
   * run(string("abra") and string("cadabra"))("cadabraabra") == Right("cadabraabra") ???
   *
   * @param s1
   * @param s2
   * @tparam A
   * @return
   */
  def and[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  /**
   * Law run(string(s))(s) == Right(s)
   * @param s
   * @return
   */
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit fn: A => Parser[String]): ParserOps[String] = ParserOps(fn(a))


  def label[A](p: Parser[A], msg: String): Parser[A]

  /**
   * Sequences two parsers, running p1 and then p2, and returns
   * the pair of their results if both succeed
   *
   * @param pa
   * @param pb
   * @tparam A
   * @tparam B
   * @return
   */
  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    pa.flatMap(a => pb.map(b => (a, b)))

  case class ParserOps[A](p: Parser[A]){
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }
}

object JSONParserImpl{
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    jsonObject(P)
  }

  def jsonNull[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON.JNull.type ] = {
    import P.*
    regex("null".r).map(_ => JSON.JNull)
  }

  def jsonNumber[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JNumber] = {
    import P.*
    double.map(JNumber.apply)
  }

  def jsonString[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JString] = {
    import P.*
    regex("\".*\"".r).map(JString.apply)
  }

  def jsonBoolean[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JBool] = {
    import P.*
    boolean.map(JBool.apply)
  }



  def jsonArray[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JArray] = {
    import P.*

    def token(s: String): Parser[String] = string(s).token

    val jArray = token("[") *> jsonValue(P).sep(token(",")).map(l => JSON.JArray.apply(l.toIndexedSeq)) <* token("]")

    jArray
  }

  /**
   *  -> { -> string : value -> (`,` string : value)* -> } ->
   * @param P
   * @tparam Err
   * @tparam Parser
   * @return
   */
  def jsonObject[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JObject] = {
    import P.*
    //TODO whitespaces
//    def wrapWithSpaces(p: Parser[String]): Parser[String] ={
//      (whitespaces.many ** p ** whitespaces.many).slice
//    }

    def token(s: String): Parser[String] = string(s).token


//    val paramName = regex("\".+\"".r)//wrapWithSpaces(regex("\".+\"".r))
//    val colonSeparator = string(":")//wrapWithSpaces(string(":"))
//    val comaSeparator = string(",")//wrapWithSpaces(string(","))
    def keyValue = (escapedQuotes <* token(":")) ** jsonValue(P) //paramName ** colonSeparator ** jsonValue(P)

    val jsonObj = token("{") *> keyValue.sep(token(",")).map(l => JObject.apply(l.toMap)) <* token("}")

//    val properties = oneProp ** (comaSeparator ** oneProp).many
//
//    val ppp = ((paramName <* token(":")) ** jsonValue(P)).sep(token(":"))
//
//    val start = string("{")//wrapWithSpaces(string("{"))
//    val end = string("}")//wrapWithSpaces(string("}"))
//    //TODO empty object ?
//    val rawObj: Parser[((String, (((String, String), JSON), List[(String, ((String, String), JSON))])), String)] = start ** properties ** end
//
//    //TODO implement with `for`
//    val jsonObj = rawObj.map{case ((_, (el: ((String, String), JSON), list: List[(String, ((String, String), JSON))])), _) => el +: list.map{ case (_, tuple) => tuple } }
//      .map(list => list.map{ case ((prop, _), json) => (prop.trim, json) }.toMap )
//      .map(JObject.apply)

    jsonObj
  }

  def jsonValue[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P.*
    jsonNull(P) | jsonNumber(P) | jsonString(P) | jsonBoolean(P) | jsonArray(P) | jsonObject(P)
  }
}
