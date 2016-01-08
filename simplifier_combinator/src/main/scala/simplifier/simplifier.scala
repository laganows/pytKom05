package simplifier

import AST._

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {
  def apply(node: Node) = simplify(node)

  def simplify(node: Node): Node = node match {
    // usuwanie duplikatow ze slownikow:
    case KeyDatumList(list) => KeyDatumList(list.foldLeft(Map.empty[Node, KeyDatum])(
      (_map, kd) => _map + (kd.key -> kd)
    ).toList.map(p => p._2))

    //case n: NodeList => SimplifyNodeList(n)
    case n: Tuple => Tuple(n.list map simplify)

    // usuwanie petli z falszywym warunkiem:
    case WhileInstr(cond, body) =>
      val sCond = simplify(cond)
      sCond match {
        case FalseConst() => EmptyInstr()
        case (_) => WhileInstr(sCond, simplify(body))
      }

    case n: IfElseInstr => SimplifyIfElseInstr(n)

    case n: IfInstr => SimplifyIfInstr(n)

    case n: IfElseExpr  => SimplifyIfElseExpr(n)

    case Assignment(Variable(x), expr) => expr match {
      case Variable(y) if x == y => EmptyInstr()
      case (_) => Assignment(Variable(x), simplify(expr))
    }

    // operacje arytmetyczne
    case n: BinExpr => SimplifyBinExpr(n)

    // wyrazenia unarne
    case n: Unary => SimplifyUnaryOpr(n)

    case n: NodeList => SimplifyNodeList(n)

    case n => n
  }

}
