package com.kuk.demo.scala2.print

object PrintableSyntax {
  implicit class PrintableOps[A](a: A){
    def format(implicit printable: Printable[A]): String =
      printable.format(a)

    def print(implicit printable: Printable[A]): Unit =
      println(format)
  }
}
