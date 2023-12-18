package com.kuk.demo.scala2.ch6.candy

import com.kuk.demo.scala2.ch6.State


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

//trait MachineServiceInterface[-I <: Input, S]{
//
//  def insert(input: I): State[S, (Int, Int)]
//
//}

trait MachineService {
//  def insert(input: Input): State[Machine, (Int, Int)] = State((m: Machine) => {
//    input match
//      case _ if m.candies <= 0 => ((m.coins, m.candies), m)
//
//      case Coin if m.locked && m.candies > 0 =>
//        val newM = m.copy(locked = false, coins = m.coins + 1)
//        ( (newM.coins, newM.candies), newM )
//
//      case Turn if !m.locked =>
//        val newM = m.copy(locked = true, candies = m.candies - 1)
//        ((newM.coins, newM.candies), newM)
//
//      case _ => ((m.coins, m.candies), m)
//
//  })

  def insert(input: Input): Machine => Machine = (m: Machine) => {
    input match
      case _ if m.candies <= 0 => m

      case Coin if m.locked && m.candies > 0 =>
        m.copy(locked = false, coins = m.coins + 1)

      case Turn if !m.locked =>
        m.copy(locked = true, candies = m.candies - 1)

      case _ => m
  }



  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] ={

//    val list = inputs match
//      case Nil => List(Init)
//      case _ => inputs
//
//    State.sequence(list.map(insert)).map( _.last )
//    val in: Input = ???
//    val mi = insert2(in)


//    val state: State[Machine, List[Unit]] = ???
//    state.flatMap(l => State.get)

//TODO review for better understanding
    for{
      _ <- State.traverse(inputs)(b => State.modify(insert(b)))
      s <- State.get
    } yield (s.coins, s.candies)

  }
}

object MachineService extends MachineService

object Application{



  @main
  def run(args: String*): Unit = {
    val service = MachineService
    val initialState = Machine(true, 5, 10)

    println(s"empty: ${service.simulateMachine(List.empty[Input]).run(initialState)}")

    val list: List[Input] = List(Coin, Turn, Coin, Coin,
      Coin, Turn, Turn,
      Coin, Turn, Turn,
      Coin, Turn)


//    val (s, t) = service.simulateMachine(list).run(initialState) //State.sequence(list.map(service.insert)).run(initialState)
//    println(s)
//    println(t)


  }
}