package com.kuk.demo.scala2.ch3

sealed trait Tree[+A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[+A](value: A) extends Tree[A]
//case object Nil extends Tree[Nothing] //??


object Tree{

  extension [A](self: Tree[A])

    def size: Int = {
      self match
        case Branch(left, right) => 1 + left.size + right.size
        case Leaf(value) => 1
    }

    def depth: Int = {
      self match
        case Branch(left, right) => 1 + left.depth.max(right.depth)
        case Leaf(value) => 1
    }

    def map[B](f: A => B): Tree[B] = {
      self match
        case Branch(left, right) => Branch(left.map(f), right.map(f))
        case Leaf(value) => Leaf(f(value))
    }

    def fold[B](fLeaf: A => B, fBranch: (B, B) => B): B =
      self match
        case Branch(left, right) =>
          fBranch(
            left.fold(fLeaf, fBranch),
            right.fold(fLeaf, fBranch)
          )
        case Leaf(value) => fLeaf(value)

    /**
     * uses `fold`` to get list of leaves
     * @return
     */
    def leaves: List[A] =
      self.fold(a => List(a), (la, ra) => la ++ ra )

    /**
     * uses recursion to get list of leaves
     *
     * @return
     */
    def leaves2: List[A] =
      self match
        case Leaf(v) => List(v)
        case Branch(l, r) => l.leaves2 ++ r.leaves2

  end extension


  private def max(a: Leaf[Int], b: Leaf[Int]): Leaf[Int] = if a.value.max(b.value) == a.value then a else b

  def maximum(as: Tree[Int]): Leaf[Int] = {
    as match
      case Branch(left, right) => max(maximum(left), maximum(right))
      case l @ Leaf(value) => l
  }

  def size2[A](as: Tree[A]): Int = {
    //z - has no effect
    as.fold( (_) => 1, (bL, bR) => 1 + bL + bR )

  }

  def maximum2(as: Tree[Int]): Leaf[Int] = {
    //z - has no effect
    as.fold( (a) => Leaf(a), (bL, bR) => max(bL, bR) )
  }

  def depth2[A](as: Tree[A]): Int = {
    as.fold( (_) => 1, (lResult, rightResult) => ( lResult + 1) max (rightResult + 1) )
  }

  def map2[A, B](as: Tree[A], f: A => B): Tree[B] = {
    as.fold[Tree[B]]( (a: A) => Leaf(f(a)), (lResult, rResult) => Branch(lResult, rResult) )
  }
}
