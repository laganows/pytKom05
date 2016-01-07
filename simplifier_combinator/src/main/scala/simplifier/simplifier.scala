package simplifier

import AST._
import math.pow

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {

///////////////////////////////////////////////////////////////////////////////////////////////////
  def apply(node: Node) = simplify(node)

  def simplify(node: Node): Node = node match {
    // usuwanie duplikatow ze slownikow:
    case KeyDatumList(list) => KeyDatumList(list.foldLeft(Map.empty[Node, KeyDatum])(
      (_map, kd) => _map + (kd.key -> kd)
    ).toList.map(p => p._2))

    //case n: NodeList => SimplifyNodeList(n)
    case n: Tuple => Tuple(n.list map simplify)

    // konkatenacja tupli i list na samym poczatku (albowiem j.w.):
    case BinExpr("+", Tuple(l1), Tuple(l2)) => Tuple((l1 ++ l2) map simplify)
    case BinExpr("+", ElemList(l1), ElemList(l2)) => ElemList((l1 ++ l2) map simplify)

    // usuwanie petli z falszywym warunkiem:
    case WhileInstr(cond, body) =>
      val sCond = simplify(cond)
      sCond match {
        case FalseConst() => EmptyInstr()
        case _            => WhileInstr(sCond, simplify(body))
      }

    case IfElseInstr(cond, left, right) =>
      val sCond = simplify(cond)
      sCond match {
        case TrueConst()  => simplify(left)
        case FalseConst() => simplify(right)
        case _            => IfElseInstr(sCond, simplify(left), simplify(right))
      }

    case IfInstr(cond, left) =>
      val sCond = simplify(cond)
      sCond match {
        case TrueConst()  => simplify(left)
        case FalseConst() => EmptyInstr()
        case _            => IfInstr(sCond, simplify(left))
      }

    case IfElseExpr(cond, left, right) =>
      val sCond = simplify(cond)
      sCond match {
        case TrueConst()  => simplify(left)
        case FalseConst() => simplify(right)
        case _            => IfElseExpr(sCond, simplify(left), simplify(right))
      }

    case Assignment(Variable(x), expr) => expr match {
      case Variable(y) if x == y => EmptyInstr()
      case _                     => Assignment(Variable(x), simplify(expr))
    }

    // <ewaluacja wyrazen> -------------------------------------------------------------------------------
    case n: BinExpr => SimplifyBinExpr(n)

    // <upraszczanie wyrazen unarnych> --------------------------------------------------------------
    case n: Unary => SimplifyUnaryOpr(n)

    case n: NodeList => SimplifyNodeList(n)

    // jesli mamy liste node'ow, to upraszczamy kazdy element z osobna:
    // w pozostalych przypadkach nie da sie juz nic uproscic:
    case n => n
  }

}
