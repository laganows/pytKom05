package simplifier
import AST._

object SimplifyIfElseInstr {
  def apply(ifStatement: IfElseInstr): Node = {
    (ifStatement.cond, ifStatement.left, ifStatement.right) match {
      case (condition, left, right) =>
        val simplifiedCondition = Simplifier(condition)
        simplifiedCondition match {
          case FalseConst() => Simplifier(right)
          case TrueConst() => Simplifier(left)
          case (_) => IfElseInstr(simplifiedCondition, Simplifier(left), Simplifier(right))
        }
    }
  }
}

object SimplifyIfInstr {
  def apply(ifStatement: IfInstr): Node = {
    (ifStatement.cond, ifStatement.left) match {
      case (condition, left) =>
        val simplifiedCondition = Simplifier(condition)
        simplifiedCondition match {
          case TrueConst() => Simplifier(left)
          case FalseConst() => EmptyInstr()
          case (_) => IfInstr(simplifiedCondition, Simplifier(left))
        }
    }
  }
}


object SimplifyIfElseExpr {
  def apply(ifStatement: IfElseExpr): Node = {
    (ifStatement.cond, ifStatement.left, ifStatement.right) match {
      case (condition, left, right) =>
        val simplifiedCondition = Simplifier(condition)
        simplifiedCondition match {
          case TrueConst()  => Simplifier(left)
          case FalseConst() => Simplifier(right)
          case _ => IfElseExpr(simplifiedCondition, Simplifier(left), Simplifier(right))
        }
    }
  }
}
