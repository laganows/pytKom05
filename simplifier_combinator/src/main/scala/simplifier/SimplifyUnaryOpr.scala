package simplifier
import AST._
import math.pow


object SimplifyUnaryOpr {
  def apply(expr: Unary): Node = {
    (expr.op, expr.expr) match {
      case ("not", expr) => expr match {
        case BinExpr("==", left, right) => Simplifier(BinExpr("!=", left, right))
        case BinExpr("!=", left, right) => Simplifier(BinExpr("==", left, right))
        case BinExpr("<=", left, right) => Simplifier(BinExpr(">",  left, right))
        case BinExpr(">=", left, right) => Simplifier(BinExpr("<",  left, right))
        case BinExpr("<", left, right)  => Simplifier(BinExpr(">=", left, right))
        case BinExpr(">", left, right)  => Simplifier(BinExpr("<=", left, right))

        case TrueConst()                => FalseConst()
        case FalseConst()               => TrueConst()

        case Unary("not", expr2)        => Simplifier(expr2) // double negation

        case expr2                      => Unary("not", Simplifier(expr2))
      }


      case ("-", expr) => expr match {
        case Unary("-", expr2) => Simplifier(expr2)
        // tutaj jeszcze tak naprawde czesc ewaluacji, ale juz zeby nie bylo az takiej redundancji tych kejsow...
        case IntNum(x)         => IntNum(-x)
        case FloatNum(x)       => FloatNum(-x)
        case expr2             => Unary("-", Simplifier(expr2))
      }

      case (_, _) => expr

    }



  }

}
