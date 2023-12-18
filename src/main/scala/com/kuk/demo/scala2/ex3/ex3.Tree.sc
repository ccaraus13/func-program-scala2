import com.kuk.demo.scala2.ch3.{Branch, Leaf, Tree}

val tree9 = Branch(
  left = Branch(
    left = Leaf(0), right = Leaf(1) //MaxDepth 3
  ),
  right = Branch(
    left = Leaf(2), right = Branch(
      left = Leaf(9), right = Leaf(4) //MaxDepth 4
    ),
  ),
)

7 / 2
7 % 2

6 / 2
6 % 2

5 / 2
5 % 2

4 / 2
4 % 2

3 / 2
3 % 2

2 / 2
2 % 2

1 / 2
1 % 2


val tree1 =  Leaf(2) //1
val tree3 =  Branch(left = Leaf(3), right = Leaf(4)) //MaxDepth 2
val tree5R =  Branch(left = Leaf(3), right = Branch(left = Leaf(3), right = Leaf(4))) //MaxDepth 3
val tree5L =  Branch(left = Branch(left = Leaf(3), right = Leaf(4)), right = Leaf(4)) //MaxDepth 3

//Tree.size(tree1)
//Tree.size2(tree1)
//
//Tree.size(tree3)
//Tree.size2(tree3)
//
//Tree.size(tree5R)
//Tree.size2(tree5R)
//
//Tree.size(tree5L)
//Tree.size2(tree5L)
//
//Tree.size(tree9)
//Tree.size2(tree9)

//Tree.maximum(tree1)
//Tree.maximum2(tree1)
//
//Tree.maximum(tree3)
//Tree.maximum2(tree3)
//
//Tree.maximum(tree5R)
//Tree.maximum2(tree5R)
//
//Tree.maximum(tree5L)
//Tree.maximum2(tree5L)
//
//Tree.maximum(tree9)
//Tree.maximum2(tree9)

//Tree.depth(tree1)
//Tree.depth2(tree1)
//
//Tree.depth(tree3)
//Tree.depth2(tree3)
//
//Tree.depth(tree5R)
//Tree.depth2(tree5R)
//
//Tree.depth(tree5L)
//Tree.depth2(tree5L)
//
//Tree.depth(tree9)
//Tree.depth2(tree9)

val f = (i: Int) => i * 2
Tree.map(tree1, f)
Tree.map2(tree1, f)

Tree.map(tree3, f)
Tree.map2(tree3, f)

Tree.map(tree5R, f)
Tree.map2(tree5R, f)

Tree.map(tree5L, f)
Tree.map2(tree5L, f)

Tree.map(tree9, f)
Tree.map2(tree9, f)
