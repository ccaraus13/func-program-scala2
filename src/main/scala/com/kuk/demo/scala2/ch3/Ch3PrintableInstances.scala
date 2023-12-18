package com.kuk.demo.scala2.ch3

import com.kuk.demo.scala2.print.Printable

import scala.collection.JavaConverters.collectionAsScalaIterableConverter

object Ch3PrintableInstances {
  implicit def TreePrintable[A]: Printable[Tree[A]] = (tree: Tree[A]) => {
    parseTree3(tree.map(_.toString)).mkString("\n")
  }

//  def parseTree[A](tree: Tree[A], depth: Int = 0, label: Char = 'A', spaces: String = " "): String = {
//    tree match
//      case Branch(l, r) => {
//        val i = (depth / 2)
//        val indent = "\t" * i
//        val indentLR = if spaces.length > 1 then spaces.substring(1) else ""//"\t" * (i - 1)
//        val labelL = (label + 1).toChar
//        val labelR = (labelL + 1).toChar
//        val strL = parseTree(l, depth - 1, labelL, indentLR)
//        val strR = parseTree(r, depth - 1, labelR, indentLR)
//        //root
//          s"$spaces$label\n" +
//          s"$spaces/\\\n" +
//          s"$strL$strR" +
//          s""
//
//
//      }
//      case Leaf(value) => {
//        val i = (depth / 2)
//        val indent = "\t" * i
//        value.toString + "\t"
//        s"$spaces$value"
//      }
//  }

//  def mapToStr[A](tree: Tree[A]): Tree[String] = tree.map(_.toString)

  private def nextLabel(currLabel: Char): Char = (currLabel + 1).toChar
//  def prevLabel(currLabel: Char): Char = (currLabel - 1).toChar
//
////  def parseTree5(tree: Tree[String], label: Char = 'A')
//
//  def parseTree2(tree: Tree[String], label: Char = 'A', parent: Option[(Tree[String], Boolean)] = None): (Char, Leaf[String]) = {
//    tree match
////      case Branch(Leaf(vl), Leaf(vr)) => {
////        val newLeaf = branchToString(tree, label)
////
////        parent.map{
////          case (Branch(l, r), true) => Branch(newLeaf, r)
////          case (Branch(l, r), false) => Branch(l, newLeaf)
////          case (Leaf(v), _) => Leaf(v)
////        }.map(newParent => parseTree2(newParent, prevLabel(label), None))
////          .getOrElse(newLeaf)
////      }
//      case Branch(Leaf(vl), Leaf(vr)) => {
//        (nextLabel(label), branchToString(tree, label))
//      }
////TODO small optimization here: one iteration less
////      case Branch(Leaf(vl), Leaf(vr)) => {
////        val newLeaf = branchToString(tree, label)
////
////        parent.map {
////            case (Branch(l, r), true) =>
////              val rLeaf = parseTree2(r, None)
////              branchToString(Branch(newLeaf, rLeaf), label)
////            case (Branch(l, r), false) =>
////              val lLeaf = parseTree2(l, None)
////              branchToString(Branch(lLeaf, newLeaf), label)
////            case (Leaf(v), _) => Leaf(v)
////          }.getOrElse(newLeaf)
////      }
//      case Leaf(value) => (label, Leaf(value))
//
//      //resulting leaf will contain both sides(left & right) + parent parsed
////      case Branch(left @ Branch(_, _), _) => parseTree2(left, nextLabel(label), Some((tree, true)))
//
//      //resulting leaf will contain both sides(left & right) + parent parsed
////      case Branch(_, right @ Branch(_, _)) => parseTree2(right, nextLabel(label), Some((tree, false)))
//
//      case Branch(left @ Branch(_, _), right) =>
//        val (newLabel, newLeaf) = parseTree2(left, label)
//        val newParent = Branch(newLeaf, right)
//        parseTree2(newParent, newLabel)
//
//
//      case Branch(left@Leaf(_), right) =>
//        val (newLabel, newLeaf) = parseTree2(right, label)
//        val newParent = Branch(left, newLeaf)
//        parseTree2(newParent, newLabel)
//
////      case Branch(left, right) => {
////        left match
////          case Branch(_, _) => {
////            //resulting leaf will contain both sides(left & right) + parent parsed
////            val leftLeaf = parseTree2(left, nextLabel(label), Some((tree, true)))
////            leftLeaf
////          }
////          case Leaf(v) => {
////            //resulting leaf will contain both sides(left & right) + parent parsed
////            val rightLeaf = parseTree2(right, nextLabel(label), Some((tree, false)))
////            rightLeaf
////          }
////      }
//
////          case Branch(left, right) => {
////            val leftLeaf = parseTree2(left,nextLabel(label), Some((tree, true)))
////            val rightLeaf = parseTree2(right,nextLabel(label), Some((tree, false)))
////            branchToString(Branch(leftLeaf, rightLeaf), label)
////          }
//
//  }
//  case class TextSize(width: Int = 0, height: Int = 0, lines: List[String] = List.empty){
//  }
//
//  def textSize(str: String): TextSize = {
//    val lines = str.lines().toList.asScala.toList
//    val width = lines.maxBy(_.length).length
//    TextSize(width, lines.size, lines)
//  }
//
//  def branchToString[A](tree: Tree[A], label: Char): Leaf[String] = {
////    val label = 'A'
//    tree match
//      case Branch(Leaf(vl), Leaf(vr)) => {
////        val leftSize = textSize(vl.toString)
////        val rightSize = textSize(vr.toString)
////
////        val commonLines = leftSize.lines.zip(rightSize.lines).map((a1,a2) => a1 + "" + a2).fold("")((a1,a2) => a1 + a2 + "\n")
////
////        val newVl = leftSize.lines.drop(leftSize.lines.size - rightSize.lines.size)
////          .fold("")((a1,a2) => a1 + a2 + "\n")
////        println(rightSize.lines.drop(rightSize.lines.size - leftSize.lines.size))
////
////        val newVr = rightSize.lines.drop(rightSize.lines.size - leftSize.lines.size)
////          .foldRight("")((a2, a1) => a1 + "\n" + (" " * (leftSize.width+2) ) + a2)
//
////        val newVr = rightSize.lines.tail.fold(rightSize.lines.head)((a1,a2) =>
////          a1 + "\n" + (" " * (leftSize.width+2) ) + a2 )
//
//        val str = s" $label\n" +
//          s" /\\\n" +
//          s"$vl  $vr" +
//          s""
//        Leaf(str)
//      }
//      case Leaf(value) => Leaf(value.toString)
//      case _ => Leaf("~~~~~~~")
//  }

  def parseTree3(tree: Tree[String], label: Char = 'A', count: Int = 0, acc: List[String] = List.empty): List[String] = {
    tree match
      case Branch(left, right) =>
        val cnt = count + 10
        val rightVal = parseTree3(right, nextLabel(label), cnt, acc)
        val str2 = (" " * count ) + label.toString
        val rAcc = rightVal :+ str2

        parseTree3(left, nextLabel(label), cnt, rAcc)
      case Leaf(v) =>
        val str1 = (" " * count) + v
        acc :+ str1
  }

}

object Play{
  import Ch3PrintableInstances.*
  @main
  def run(s: String*): Unit = {
    val tree3 = Branch(left = Leaf(1), right = Leaf(2)) //MaxDepth 2
    val tree5R = Branch(left = Leaf(1), right = Branch(left = Leaf(2), right = Leaf(3))) //MaxDepth 3
    val tree5L =  Branch(left = Branch(left = Leaf(1), right = Leaf(2)), right = Leaf(3))
    val treeBig =  Branch(
      left = Branch(
        left = Branch(
          left = Branch(left = Leaf(1), right = Leaf(2)),
          right = Leaf(3)
        ),
        right = Branch(left = Leaf(4), right = Leaf(5))
      ),
      right = Branch(
        left = Leaf(6),
        right = Branch(
          left = Branch(left = Leaf(7), right = Leaf(8)),
          right = Leaf(9)
        )
      )
    )

//    println(branchToString(tree3, "A").value)
//    println(parseTree2(tree3.map(_.toString)).value)
    println("=========most right==============")
    val tree5RStr = parseTree3(tree5R.map(_.toString)).mkString("\n")
    println(tree5RStr)

    println("=========most left==============")
    val tree5LStr = parseTree3(tree5L.map(_.toString)).mkString("\n")
    println(tree5LStr)

    println("=========BIG==============")
    val treeBigStr = parseTree3(treeBig.map(_.toString)).mkString("\n")
    println(treeBigStr)
//    println(parseTree2(treeBig.map(_.toString))._2.value)

//    println(tree5R.depth)
//    val spaces = " " * 5 * (tree5R.depth/2)
////    println(parseTree(tree3, tree3.depth-1) )
//    println(parseTree(tree5R, tree5R.depth, spaces = spaces) )
//    println(parseTree(tree5L, tree5L.depth, spaces = spaces) )
//    println(parseTree(tree5R))
//    val a = 'A'
//    val a1 = (a.toInt + 1).toChar
//    val a2 = (a1 + 1).toChar
//    val s =
//      """|dsada
//
//          |dasdas
//        |sadas
//        |""".stripMargin
//    println(s"$a - ${a1} - ${a2} - ${a2.toString*3}")
//    println(s)
  }
}
