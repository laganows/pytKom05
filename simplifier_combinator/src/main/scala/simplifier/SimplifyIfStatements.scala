package simplifier
import AST._
import math.pow


object SimplifyIfElseInstr {
  def apply(expr: IfElseInstr): Node = {
    (expr.cond, expr.left, expr.right) match {
      case (cond, left, right) =>
        val sCond = Simplifier(cond)
        sCond match {
          case TrueConst()  => Simplifier(left)
          case FalseConst() => Simplifier(right)
          case _            => IfElseInstr(sCond, Simplifier(left), Simplifier(right))
        }
    }
  }
}

object SimplifyIfInstr {
  def apply(expr: IfInstr): Node = {
    (expr.cond, expr.left) match {
      case (cond, left) =>
        val sCond = Simplifier(cond)
        sCond match {
          case TrueConst()  => Simplifier(left)
          case FalseConst() => EmptyInstr()
          case _            => IfInstr(sCond, Simplifier(left))
        }
    }
  }
}


object SimplifyIfElseExpr {
  def apply(expr: IfElseExpr): Node = {
    (expr.cond, expr.left, expr.right) match {
      case (cond, left, right) =>
        val sCond = Simplifier(cond)
        sCond match {
          case TrueConst()  => Simplifier(left)
          case FalseConst() => Simplifier(right)
          case _            => IfElseExpr(sCond, Simplifier(left), Simplifier(right))
        }
    }
  }
}
