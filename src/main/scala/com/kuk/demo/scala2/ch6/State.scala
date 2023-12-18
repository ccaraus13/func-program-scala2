package com.kuk.demo.scala2.ch6

import com.kuk.demo.scala2.ch6.State._

case class State[S, +A](run: S => (A, S)){
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = this.run(s)
    val stateB: State[S, B] = f(a)

    stateB.run(s2)
  })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](stateB: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap(a => {
      stateB.flatMap(b => unit(f(a, b)))
    })

}

object State{
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight( unit(List.empty[A]) ){ case (ra, rl) =>
      rl.map2(ra)((list, a) => a :: list)
    }
  }

  //def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)]
  //def traverse[S, A, B](list: List[A])(f: A => State[S, B]): State[S, List[B]]

  def traverse[S, A, B](list: List[A])(f: A => State[S, B]): State[S, List[B]] =
    list.foldRight(unit(List.empty[B]))( (b, acc) => f(b).map2(acc)(_ :: _) )


}
