package com.kuk.demo.scala2.print

object PrintableInstances {
  implicit val StringPrintable: Printable[String] = (a: String) => a

  implicit val IntPrintable: Printable[Int] = (a: Int) => a.toString

}
