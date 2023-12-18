
def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a, b)
//  f.curried(a)

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  a => {
    b =>
      f(a, b)
  }
//  f.curried