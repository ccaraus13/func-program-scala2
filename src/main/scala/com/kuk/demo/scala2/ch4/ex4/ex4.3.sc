import com.kuk.demo.scala2.ch4.{Option, None, Some}


def Try[A](a: => A): Option[A] = {
  try Some(a)
  catch
    case e: Exception => None
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (numberOfSpeedingTickets + age) / 32

Option.map2(Try("abc".toInt), Try("qwe".toInt))(insuranceRateQuote)
Option.map2(Try("123".toInt), Try("qwe".toInt))(insuranceRateQuote)
Option.map2(Try("123".toInt), Try("2".toInt))(insuranceRateQuote)

Option.sequence(List(Option(1), Option(2), None, Option(3)))
Option.traverse(List("1", "2", "none", "3"))(a => Try(a.toInt))

Option.sequence(List(None, Option(2), None, Option(3)))
Option.traverse(List("none", "2", "none", "3"))(a => Try(a.toInt))

Option.sequence(List(Option(1), Option(2), Option(3), None))
Option.traverse(List("1", "2", "3", "no"))(a => Try(a.toInt))

Option.sequence(List(Option(1), Option(2), Option(3), Option(4)))
Option.traverse(List("1", "2", "3", "4"))(a => Try(a.toInt))