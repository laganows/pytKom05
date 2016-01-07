package simplifier
import AST._
import math.pow


object SimplifyNodeList {
  def apply(expr: NodeList): Node = {
    (expr.list) match {
      case (list) => list match {
        case Nil => EmptyInstr()
        case (nl::Nil) => nl match {
          case EmptyInstr() => EmptyInstr()
          case NodeList(l)  => Simplifier(NodeList(l map Simplifier.simplify)) // zostawiamy tylko jeden layer node listow
          case n            =>
            val sN = Simplifier(n)
            if (sN != n) Simplifier(NodeList(List(Simplifier(n)))) else NodeList(List(sN))
        }
        case _   =>
          val sList = (list map Simplifier.simplify).foldRight(List.empty[Node])(
            (n: Node, list2: List[Node]) => list2 match {
              case Nil   => List(n)
              case x::xs => (n, x) match {
                case (Assignment(lvalN, rvalN), Assignment(lvalX, rvalX)) =>
                  if (lvalN == lvalX) x::xs else n::x::xs
                case _ => x::xs
              }
            }
          ).reverse

          val change = (list.length != sList.length) && ((list zip sList) exists (p => p._1 != p._2))
          if (change) Simplifier(NodeList(sList)) else NodeList(sList)
      }

    }



  }

}
