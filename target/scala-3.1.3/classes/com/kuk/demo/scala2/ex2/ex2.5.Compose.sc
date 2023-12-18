


def compose1[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))

def compose2[A, B, C](f: B => C, g: A => B): A => C =
  f.compose(g)

def compose3[A, B, C](f: B => C, g: A => B): A => C =
  g.andThen(f)