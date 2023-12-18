
class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}


//case class CTreeNode(_value: Int, _left: Option[CTreeNode] = None, _right: Option[CTreeNode] = None)
//
//object CTreeNode{
//  def apply(tree: Option[TreeNode]): Option[CTreeNode] =
//    tree.map(aTree => CTreeNode( aTree.value, apply(Option(aTree.left)), apply(Option(aTree.right)) ))
//
//}

object Solution {
  object TreeNode {
    def apply(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null): TreeNode = new TreeNode(_value, _left, _right)
    def unapply(tree: TreeNode): (Int, TreeNode, TreeNode) = Option(tree).map(t => (t.value, t.left, t.right)).getOrElse((0, null, null))
  }

  def invertTree(root: TreeNode): TreeNode = {
    root match {
      //leef
      case TreeNode(v, null, null) => TreeNode(v, null, null)
      case TreeNode(v, left, null) => TreeNode(v, null, invertTree( left ))
      case TreeNode(v, null, right) => TreeNode(v, invertTree(right), null)
      case TreeNode(v, left, right) => TreeNode(v, invertTree( right ), invertTree( left ) )
    }
  }

}

Solution.invertTree(TreeNode(4, TreeNode(1), TreeNode(3))).left.value