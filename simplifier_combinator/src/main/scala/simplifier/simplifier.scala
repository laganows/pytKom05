package simplifier

import AST._
import math.pow

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {
  def apply(node: Node) = simplify(node)

  def simplify(node: Node): Node = node match {
    case node: Unary => UnarySimplifier(node)
    // case node: BinExpr => BinarySimplifier(node)
    // case node: IfElseExpr => IfElseExprSimplifier(node)
    // case node: Assignment => AssignmentSimplifier(node)
    // case node: Subscription => SubscriptionSimplifier(node)
    // case node: KeyDatum => KeyDatumSimplifier(node)
    // case node: GetAttr => GetAttrSimplifier(node)
    // case node: IfInstr => IfInstrSimplifier(node)
    // case node: IfElseInstr => IfElseInstrSimplifier(node)
    // case node: WhileInstr => WhileInstrSimplifier(node)
    // case node: ReturnInstr => ReturnInstrSimplifier(node)
    // case node: PrintInstr => PrintInstrSimplifier(node)
    // case node: FunCall => FunCallSimplifier(node)
    // case node: FunDef => FunDefSimplifier(node)
    // case node: LambdaDef => LambdaDefSimplifier(node)
    // case node: ClassDef => ClassDefSimplifier(node)
    // case node: NodeList => NodeListSimplifier(node)
    // case node: KeyDatumList => KeyDatumListSimplifier(node)
    // case node: ElemList => ElemListSimplifier(node)
    // case node: Tuple => TupleSimplifier(node)
    case default => node
  }
}

object UnarySimplifier {
  def apply(node: Unary) = {
    val expr = Simplifier(node.expr)

    case Unary("not", expr) => expr match {
      case BinExpr("==", left, right) => simplify(BinExpr("!=", left, right))
      case BinExpr("!=", left, right) => simplify(BinExpr("==", left, right))
      case BinExpr("<=", left, right) => simplify(BinExpr(">",  left, right))
      case BinExpr(">=", left, right) => simplify(BinExpr("<",  left, right))
      case BinExpr("<", left, right)  => simplify(BinExpr(">=", left, right))
      case BinExpr(">", left, right)  => simplify(BinExpr("<=", left, right))

      case TrueConst()                => FalseConst()
      case FalseConst()               => TrueConst()

      case Unary("not", expr2)        => simplify(expr2) // double negation

      case expr2                      => Unary("not", simplify(expr2))
    }


    case Unary("-", expr) => expr match {
      case Unary("-", expr2) => simplify(expr2)
      // tutaj jeszcze tak naprawde czesc ewaluacji, ale juz zeby nie bylo az takiej redundancji tych kejsow...
      case IntNum(x)         => IntNum(-x)
      case FloatNum(x)       => FloatNum(-x)
      case expr2             => Unary("-", simplify(expr2))
    }
  }
}



