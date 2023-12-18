import com.kuk.demo.scala2.ch4.{Option, None, Some}

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)
/**
 * m - mean
 * @param xs
 * @return math.pow(x-m, 2)
 */
def variance(xs: Seq[Double]): Option[Double] = {
//  mean(xs).flatMap( m => xs.foldLeft(None: Option[Double])((_, x) => Option(math.pow(x - m, 2))) )
  mean(xs).flatMap( m => mean(xs.map(x => math.pow(x - m, 2))) )
}



variance(Nil)
variance(Seq(1))
variance(Seq(1,2,3,4))
variance(Seq(-4, -3, -2, -1, 0, 4,5,6,7))