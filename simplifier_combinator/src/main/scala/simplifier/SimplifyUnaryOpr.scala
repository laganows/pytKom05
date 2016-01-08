package simplifier
import AST._

object SimplifyUnaryOpr {
  def apply(unaryOpr: Unary): Node = {
    (unaryOpr.op, unaryOpr.expr) match {
      case ("not", unaryOpr) => unaryOpr match {
        case BinExpr("==", left, right) => Simplifier(BinExpr("!=", left, right))
        case BinExpr("!=", left, right) => Simplifier(BinExpr("==", left, right))
        case BinExpr("<=", left, right) => Simplifier(BinExpr(">",  left, right))
        case BinExpr(">=", left, right) => Simplifier(BinExpr("<",  left, right))
        case BinExpr("<", left, right) => Simplifier(BinExpr(">=", left, right))
        case BinExpr(">", left, right) => Simplifier(BinExpr("<=", left, right))
        case TrueConst() => FalseConst()
        case FalseConst() => TrueConst()
        case Unary("not", expr2) => Simplifier(expr2)
        case expr2 => Unary("not", Simplifier(expr2))
      }

      case ("-", unaryOpr) => unaryOpr match {
        case Unary("-", expr2) => Simplifier(expr2)
        case IntNum(x) => IntNum(-x)
        case FloatNum(x) => FloatNum(-x)
        case expr2 => Unary("-", Simplifier(expr2))
      }
      case (_, _) => unaryOpr
    }
  }
}
