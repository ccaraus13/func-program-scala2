import com.kuk.demo.scala2.ch4.{Either, Left, Right}


def Try[A](a: => A): Either[String, A] = {
  try Right(a)
  catch
    case e: Exception => Left(e.getMessage)
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (numberOfSpeedingTickets + age) / 32

Try("abc".toInt).map2(Try("qwe".toInt))(insuranceRateQuote)
Try("abc".toInt).map2(Try("123".toInt))(insuranceRateQuote)
Try("123".toInt).map2(Try("qwe".toInt))(insuranceRateQuote)
Try("123".toInt).map2(Try("2".toInt))(insuranceRateQuote)