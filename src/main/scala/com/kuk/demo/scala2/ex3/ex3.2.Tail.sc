import scala.annotation.tailrec

def tail[A](as: List[A]): List[A] = as match
  case Nil => Nil
  case x :: xs => xs

def setHead[A](head: A, as: List[A]): List[A] = as match
  case Nil => head :: Nil
  case x :: xs => head :: xs

@tailrec
def drop[A](n: Int, as: List[A]): List[A] = {

  if( n <= 0) as
  else if( n >= as.length ) Nil
  else as match
      case Nil => Nil
      case x :: xs => drop(n - 1, xs)
}

@tailrec
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  l match
    case x :: xs if f(x) => dropWhile(xs, f)
    case xs => xs
}

def init[A](as: List[A]): List[A] = {

  @tailrec
  def loop(as: List[A], acc: List[A] = Nil): List[A] = as match
    case Nil => acc
    case x :: Nil => acc
    case x :: xs => loop(xs, acc :+ x)

 loop(as)
}


def length[A](as: List[A]): Int = as.foldRight(0)((_, acc) => acc + 1)

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  as match
    case Nil => z
    case x :: xs => f( x, foldRight(xs, z)(f) )
}

def foldRightTR[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  @tailrec
  def loop(as: List[A], acc: B = z): B = {
    as match
      case Nil => acc
      case x :: xs => loop(xs, f(x, acc))
  }

  loop(as)
}

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  @tailrec
  def loop(as: List[A], acc: B = z): B = {
    as match
      case Nil => acc
      case x :: xs => loop(xs, f(acc, x))
  }

  loop(as)
}

def foldLeft_WithFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
  foldRight(as, z)( (a: A, b: B) => f(b, a) )
}

def foldRight_WithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  foldLeft(as, z)( (b: B, a: A) => f(a, b) )
}

def sumFL(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
def productFL(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
def lengthFL[A](as: List[A]): Int = foldLeft(as, 0)( (acc, _) => acc + 1 )

/**
 * ex 3.14
 * @param h
 * @param as
 * @tparam A
 * @return
 */
def append_FoldLeft[A](h: A, as: List[A]): List[A] = {
  foldLeft(List(as), List(h))( (list, a) => a ++ list)
}

/**
 * ex 3.14
 *
 * @param h
 * @param as
 * @tparam A
 * @return
 */
def append_FoldRight[A](h: A, as: List[A]): List[A] = {
  foldRight(List(as), List(h))((a, list) => a ++ list)
}

def concat[A](list: List[List[A]]): List[A] = {
  foldLeft(list, Nil: List[A])( (acc, a) => {
    println(s"$a ++ $acc <> ${a ++ acc}")
    a ++ acc
  } )
}

//append_FoldRight(13, List(1,2,3,4))
//append_FoldRight(13, Nil)

concat(List(List(1), Nil, List(1,2), List(3,4,5), List(6), Nil))
List(1, 2, 1) ++ List(3,4,5)