
// X* => X*
def r[X](list: List[X]): List[X] = ???
// A => A1
def a[A, A1](a: A): A1 = ???

/*
//???
map( map(y)(g) )(f) == map(y)( f compose g) //???

(1)map(y)(id) == y
(2)map(y)(f) == f(y)
(3)map(y)(g) == g(y)

map( map(y)(g) )(f) =(3)=> map( g(y) )(f) =(2)=> f(g(y)) => (f compose g) (y) =(2)=> map(y)(f compose g)


 */