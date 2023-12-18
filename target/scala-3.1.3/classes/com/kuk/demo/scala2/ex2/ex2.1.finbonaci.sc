import scala.annotation.tailrec

/**
 * The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5
 *
 * @param n
 * @return
 */
def fib(n: Int): Int = {

  @tailrec
  def go(i: Int = 1, acc1: Int = 0, acc2: Int = 1): Int = {
      if(n == 0)
        0
      else if ( i < n )
        go(i + 1, acc2, acc1 + acc2)
      else
        acc2
  }

  go()
}

def printFib(n: Int): Unit = {
  println(s"Fibonacci $n: ${fib(n)}")
}

printFib(0)
printFib(1)
printFib(2)
printFib(3)
printFib(4)
printFib(5)
printFib(6)
printFib(7)
printFib(19)
printFib(-19)