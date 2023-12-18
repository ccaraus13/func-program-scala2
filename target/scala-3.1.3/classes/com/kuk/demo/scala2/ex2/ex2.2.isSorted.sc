
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
//  val p = (a: A, b: A) => a == b
//  as.sortWith(ordered).corresponds(as)(p)

  def loop(idx: Int = 0): Boolean = {
    //0 or 1 array length
    if (as.length <= 1)
      true
    //last element reached
    else if ( (idx + 1) >= as.length )
      ordered( as(idx - 1), as(idx) )
    else if( ordered(as(idx), as(idx + 1)) )
      loop(idx + 1)
    else
      false
  }

 loop()
}

val array = Array(1,2,30,4,50,6,7,8)
val pAsc = (a: Int, b: Int) => a < b
val pDesc = (a: Int, b: Int) => a > b
val pEq = (a: Int, b: Int) => a == b

val ascArr = array.sortWith(pAsc)
val descArr = array.sortWith(pDesc)


println(s"Is Sorted ASC ${ascArr.mkString("Array(", ", ", ")")}? => ${isSorted(ascArr, pAsc)}")
println(s"Is Sorted DESC ${ascArr.mkString("Array(", ", ", ")")}? => ${isSorted(ascArr, pDesc)}")
println()
println(s"Is Sorted ASC ${descArr.mkString("Array(", ", ", ")")}? => ${isSorted(descArr, pAsc)}")
println(s"Is Sorted DESC ${descArr.mkString("Array(", ", ", ")")}? => ${isSorted(descArr, pDesc)}")
println()
println(s"Is Sorted ASC ${array.mkString("Array(", ", ", ")")}? => ${isSorted(array, pAsc)}")
println(s"Is Sorted DESC ${array.mkString("Array(", ", ", ")")}? => ${isSorted(array, pDesc)}")