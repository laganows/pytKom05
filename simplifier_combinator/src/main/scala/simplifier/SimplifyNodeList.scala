package simplifier
import AST._

object SimplifyNodeList {
  def apply(nodeList: NodeList): Node = {
    (nodeList.list) match {
      case (list) => list match {
        case Nil => BlankInstruction()
        case (snowflake::Nil) => snowflake match {
          case BlankInstruction() => BlankInstruction()
          case NodeList(l) => Simplifier(NodeList(l map Simplifier.simplify)) 
          case n =>
            val simplifiedElement = Simplifier(n)
            if (simplifiedElement != n) Simplifier(NodeList(List(Simplifier(n)))) else NodeList(List(simplifiedElement))
        }
        case (_) =>
          val sList = (list map Simplifier.simplify).foldRight(List.empty[Node])(
            (n: Node, list2: List[Node]) => list2 match {
              case Nil => List(n)
              case x::xs => (n, x) match {
                case (Assignment(leftValueNode, rightValueNode), Assignment(leftValueX, rightValueX)) =>
                  if (leftValueNode == leftValueX) x::xs else n::x::xs
                case (_) => x::xs
              }
            }
          ).reverse

          val change = (list.length != sList.length) && ((list zip sList) exists (p => p._1 != p._2))
          if (change) Simplifier(NodeList(sList)) else NodeList(sList)
      }
    }
  }
}
