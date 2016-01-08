package simplifier
import AST._
import math.pow


object SimplifyBinExpr {
  def apply(expr: BinExpr): Node = {
    (expr.op, expr.left, expr.right) match {
      case ("+", Tuple(l1), Tuple(l2)) => Tuple((l1 ++ l2) map Simplifier.simplify)
      case ("+", ElemList(l1), ElemList(l2)) => ElemList((l1 ++ l2) map Simplifier.simplify)
      case (op, IntNum(x), IntNum(y)) =>
        op match {
          case "+" => IntNum(x + y)
          case "-" => IntNum(x - y)
          case "*" => IntNum(x * y)
          case "/" => IntNum(x / y)
          case "%" => IntNum(x % y)
          case "**" => IntNum(pow(x.toDouble, y.toDouble).toInt)

          case "==" => if (x == y) TrueConst() else FalseConst()
          case "!=" => if (x != y) TrueConst() else FalseConst()
          case ">=" => if (x >= y) TrueConst() else FalseConst()
          case "<=" => if (x <= y) TrueConst() else FalseConst()
          case ">" => if (x > y)   TrueConst() else FalseConst()
          case "<" => if (x < y)   TrueConst() else FalseConst()
        }

      case (op, FloatNum(x), FloatNum(y)) =>
        op match {
          case "+"  => FloatNum(x + y)
          case "-"  => FloatNum(x - y)
          case "*"  => FloatNum(x * y)
          case "/"  => FloatNum(x / y)
          case "%"  => FloatNum(x % y)
          case "**" => FloatNum(pow(x, y))

          case "==" => if (x == y) TrueConst() else FalseConst()
          case "!=" => if (x != y) TrueConst() else FalseConst()
          case ">=" => if (x >= y) TrueConst() else FalseConst()
          case "<=" => if (x <= y) TrueConst() else FalseConst()
          case ">"  => if (x > y)  TrueConst() else FalseConst()
          case "<"  => if (x < y)  TrueConst() else FalseConst()
        }

      case (op, IntNum(x), FloatNum(y)) =>
        op match {
          case "+"  => FloatNum(x + y)
          case "-"  => FloatNum(x - y)
          case "*"  => FloatNum(x * y)
          case "/"  => FloatNum(x / y)
          case "%"  => FloatNum(x % y)
          case "**" => FloatNum(pow(x.toDouble, y))

          case "==" => if (x == y) TrueConst() else FalseConst()
          case "!=" => if (x != y) TrueConst() else FalseConst()
          case ">=" => if (x >= y) TrueConst() else FalseConst()
          case "<=" => if (x <= y) TrueConst() else FalseConst()
          case ">"  => if (x > y)  TrueConst() else FalseConst()
          case "<"  => if (x < y)  TrueConst() else FalseConst()
        }

      case (op, FloatNum(x), IntNum(y)) =>
        op match {
          case "+"  => FloatNum(x + y)
          case "-"  => FloatNum(x - y)
          case "*"  => FloatNum(x * y)
          case "/"  => FloatNum(x / y)
          case "%"  => FloatNum(x % y)
          case "**" => FloatNum(pow(x, y.toDouble))

          case "==" => if (x == y) TrueConst() else FalseConst()
          case "!=" => if (x != y) TrueConst() else FalseConst()
          case ">=" => if (x >= y) TrueConst() else FalseConst()
          case "<=" => if (x <= y) TrueConst() else FalseConst()
          case ">"  => if (x > y)  TrueConst() else FalseConst()
          case "<"  => if (x < y)  TrueConst() else FalseConst()
        }

      case ("==", x, y) if x == y => TrueConst()
      case (">=", x,y)  if x == y => TrueConst()
      case ("<=", x,y)  if x == y => TrueConst()
      case ("!=", x,y)  if x == y => FalseConst()
      case ("<", x,y)   if x == y => FalseConst()
      case (">", x,y)   if x == y => FalseConst()
      case ("or", x ,y) if x == y => x
      case ("and", x,y) if x == y => x

      case ("+", BinExpr("+", BinExpr("+", BinExpr("*", x1, y1), BinExpr("*", x2, y2)), BinExpr("*", x3, y3)), BinExpr("*", x4, y4))
        => Simplifier(BinExpr("+", BinExpr("+", BinExpr("*", x1, y1), BinExpr("*", x2, y2)), BinExpr("+", BinExpr("*", x3, y3), BinExpr("*", x4, y4))))


      case ("-", left, right)    => (Simplifier(left), Simplifier(right)) match {
        case (exprL, exprR) if exprL == exprR => IntNum(0)
        case (expr, IntNum(n)) if n == 0  => expr
        case (IntNum(n), expr) if n == 0  => Simplifier(Unary("-", expr))
        case (expr, FloatNum(n)) if n == 0 => expr
        case (FloatNum(n), expr) if n == 0 => Simplifier(Unary("-", expr))

        // distributive properties of "*":
        case (BinExpr("*", l, r), expr) if expr == l => Simplifier(BinExpr("*", BinExpr("-", r, IntNum(1)), l))
        case (BinExpr("*", l, r), expr) if expr == r => Simplifier(BinExpr("*", BinExpr("-", l, IntNum(1)), r))

        case (e1@BinExpr("*", l1, r1), e2@BinExpr("*", l2, r2)) =>
          if (l1 == l2) BinExpr("*", BinExpr("+", r1, r2), l1)
          else if (r1 == r2) BinExpr("*", BinExpr("-", l1, l2), r1)
          else if (l1 == r2) BinExpr("*", BinExpr("-", r1, l2), l1)
          else if (r1 == l2) BinExpr("*", BinExpr("-", l1, r2), r1)
          else {
            val s1 = Simplifier(e1)
            val s2 = Simplifier(e2)
            if (s1 != e1 || s2 != e2) Simplifier(BinExpr("-", s1, s2)) else BinExpr("-", s1, s2)
          }

        // distributive properties of "/":
        case (e1@BinExpr("/", l1, r1), e2@BinExpr("/", l2, r2)) =>
          if (r1 == r2) BinExpr("/", BinExpr("-", l1, l2), r1)
          else {
            val s1 = Simplifier(e1)
            val s2 = Simplifier(e2)
            if (s1 != e1 || s2 != e2) Simplifier(BinExpr("-", s1, s2)) else BinExpr("-", s1, s2)
          }

        // wzory skr. mnozenia:
        case (e1@BinExpr("**", BinExpr(op1, x1, y1), IntNum(a)), e2@BinExpr("**", BinExpr(op2, x2, y2), IntNum(b)))
          if x1 == x2 && y1 == y2 && a == 2 && b == 2 =>
            if (op1 == "+" && op2 == "-") BinExpr("*", BinExpr("*", x1, IntNum(4)), y1)
            else if (op1 == "-" && op2 == "+") Unary("-", BinExpr("*", BinExpr("*", x1, IntNum(4)), y1))
            else BinExpr("-", Simplifier(e1), Simplifier(e2))

        case (BinExpr("-", BinExpr("**", BinExpr("+", x1, y1), IntNum(a)), BinExpr("**", x2, IntNum(b))), BinExpr("*", BinExpr("*", x3, IntNum(c)), y3))
          if a == 2 && b == 2 && c == 2 && x1 == x2 && ((x1 == x3 && y1 == y3) || (x1 == y3 && y1 == x3)) =>
            Simplifier(BinExpr("**", y1, IntNum(2)))
        case (BinExpr("-", BinExpr("**", BinExpr("+", x1, y1), IntNum(a)), BinExpr("**", x2, IntNum(b))), BinExpr("*", BinExpr("*", x3, IntNum(c)), y3))
          if a == 2 && b == 2 && c == 2 && y1 == x2 && ((x1 == x3 && y1 == y3) || (x1 == y3 && y1 == x3)) =>
          BinExpr("**", x1, IntNum(2))

        // commutative properties:
        case (e@BinExpr("+", exprL, exprR), expr) =>
          if (exprL == expr) Simplifier(exprR)
          else if (exprR == expr) Simplifier(exprL)
          else BinExpr("-", Simplifier(e), Simplifier(expr))
        case (expr, e@BinExpr("+", exprL, exprR)) =>
          if (exprL == expr) Simplifier(Unary("-", exprR))
          else if (exprR == expr) Simplifier(Unary("-", exprL))
          else BinExpr("-", Simplifier(expr), Simplifier(e))

        case (exprL, exprR)      =>
          val sL = Simplifier(exprL)
          val sR = Simplifier(exprR)
          if (sL != exprL || sR != exprR) Simplifier(BinExpr("-", sL, sR)) else BinExpr("-", sL, sR)
      }

      case ("+", left, right)    => (Simplifier(left), Simplifier(right)) match {
        case (expr, IntNum(n)) if n == 0  => expr
        case (IntNum(n), expr) if n == 0  => expr
        case (expr, FloatNum(n)) if n == 0 => expr
        case (FloatNum(n), expr) if n == 0 => expr
        case (Unary("-", exprU), expr) => Simplifier(BinExpr("-", expr, exprU))
        case (expr, Unary("-", exprU)) => Simplifier(BinExpr("-", expr, exprU))

        // wzory sk. mnozenia:
        case (BinExpr("+", BinExpr("**", x1, IntNum(a)), BinExpr("*", BinExpr("*", x2, IntNum(b)), y2)), BinExpr("**", y3, IntNum(c)))
          if a == 2 && b == 2 && c == 2 && (x1 == x2 && y2 == y3) =>
            Simplifier(BinExpr("**", BinExpr("+", x1, y2), IntNum(2)))
        case (BinExpr("+", BinExpr("**", x1, IntNum(a)), BinExpr("*", BinExpr("*", x2, IntNum(b)), y2)), BinExpr("**", y3, IntNum(c)))
          if a == 2 && b == 2 && c == 2 && (x1 == y2 && x2 == y3) =>
            Simplifier(BinExpr("**", BinExpr("+", x1, y3), IntNum(2)))

        case (e1@BinExpr("*", l1, r1), e2@BinExpr("*", l2, r2)) =>
          if (l1 == l2) BinExpr("*", BinExpr("+", r1, r2), l1)
          else if (r1 == r2) BinExpr("*", BinExpr("+", l1, l2), r1)
          else if (l1 == r2) BinExpr("*", BinExpr("+", r1, l2), l1)
          else if (r1 == l2) BinExpr("*", BinExpr("+", l1, r2), r1)
          else {
            val s1 = Simplifier(e1)
            val s2 = Simplifier(e2)
            if (s1 != e1 || s2 != e2) Simplifier(BinExpr("+", s1, s2)) else BinExpr("+", s1, s2)
          }

        case (exprL, exprR)      =>
          val sL = Simplifier(exprL)
          val sR = Simplifier(exprR)
          if (sL != exprL || sR != exprR) Simplifier(BinExpr("+", sL, sR)) else BinExpr("+", sL, sR)
      }

      case ("*", left, right)    => (left, right) match {
        case (expr, IntNum(n))   => if (n == 1) Simplifier(expr) else if (n == 0) IntNum(0) else {
          val s = Simplifier(expr)
          if (s != expr) Simplifier(BinExpr("*", s, IntNum(n))) else BinExpr("*", s, IntNum(n))
        }
        case (IntNum(n), expr)   => if (n == 1) Simplifier(expr) else if (n == 0) IntNum(0)   else {
          val s = Simplifier(expr)
          if (s != expr) Simplifier(BinExpr("*", s, IntNum(n))) else BinExpr("*", s, IntNum(n))
        }
        case (expr, FloatNum(n)) => if (n == 1) Simplifier(expr) else if (n == 0) FloatNum(0) else {
          val s = Simplifier(expr)
          if (s != expr) Simplifier(BinExpr("*", s, FloatNum(n))) else BinExpr("*", s, FloatNum(n))
        }
        case (FloatNum(n), expr) => if (n == 1) Simplifier(expr) else if (n == 0) FloatNum(0) else {
          val s = Simplifier(expr)
          if (s != expr) Simplifier(BinExpr("*", s, FloatNum(n))) else BinExpr("*", s, FloatNum(n))
        }

        // power laws:
        case (BinExpr("**", leftL, rightL), BinExpr("**", leftR, rightR)) if leftL == leftR =>
          Simplifier(BinExpr("**", leftL, BinExpr("+", rightL, rightR))) // TODO: czy potrzebny wewnetrzny Simplifier?

        case (expr, BinExpr("/", exprNum, exprDenom)) => Simplifier(BinExpr("/", BinExpr("*", expr, exprNum), exprDenom))
        // case (BinExpr("/", exprNum, exprDenom), expr) => Simplifier(BinExpr("/", BinExpr("*", expr, exprNum), exprDenom))
        case (exprL, exprR)      =>
          val sL = Simplifier(exprL)
          val sR = Simplifier(exprR)
          if (sL != exprL || sR != exprR) Simplifier(BinExpr("*", sL, sR)) else BinExpr("*", sL, sR)
      }

      case ("/", left, right) => (left, right) match {
        case (exprL, exprR) if exprL == exprR => IntNum(1)
        // nazwy ponizej moga nie byc najbardziej czytelne, ale to raczej przez specyfike problemu :)
        case (exprNum, BinExpr("/", denomNum, denomDenom)) =>
          val sNum = Simplifier(exprNum)
          val sDenomNum = Simplifier(denomNum)
          val sDenomDenom = Simplifier(denomDenom)
          sNum match {
            case BinExpr("/", sNumNum, sNumDenom) =>
              Simplifier(BinExpr("/", BinExpr("*", sNumNum, sDenomDenom), BinExpr("*", sNumDenom, sDenomNum)))
            case expr => Simplifier(BinExpr("/", BinExpr("*", expr, sDenomDenom), sDenomNum))
          }

        // power laws:
        case (BinExpr("**", leftL, rightL), BinExpr("**", leftR, rightR)) if leftL == leftR =>
          Simplifier(BinExpr("**", leftL, BinExpr("-", rightL, rightR))) // TODO: czy potrzebny wewnetrzny Simplifier?

        case (expr, IntNum(n))   if n == 1 => Simplifier(expr)
        case (expr, FloatNum(n)) if n == 1 => Simplifier(expr)
        case (exprL, exprR) =>
          val sL = Simplifier(exprL)
          val sR = Simplifier(exprR)
          if (sL != exprL || sR != exprR) Simplifier(BinExpr("/", sL, sR)) else BinExpr("/", sL, sR)
      }

      case ("**", left, right)    => (Simplifier(left), Simplifier(right)) match {
        case (expr, IntNum(n))   => if (n == 1) Simplifier(expr) else if (n == 0) IntNum(1) else {
          val s = Simplifier(expr)
          if (s != expr) Simplifier(BinExpr("**", s, IntNum(n))) else BinExpr("**", s, IntNum(n))
        }
        // power laws:
        case (BinExpr("**", l, r), expr) => Simplifier(BinExpr("**", l, BinExpr("*", r, expr)))

        case (exprL, exprR)      =>
          val sL = Simplifier(exprL)
          val sR = Simplifier(exprR)
          if (sL != exprL || sR != exprR) Simplifier(BinExpr("**", sL, sR)) else BinExpr("**", sL, sR)
      }

      case ("and", left, right) =>
        val sLeft = Simplifier(left)
        val sRight = Simplifier(right)
        (sLeft, sRight) match {
          case (_, FalseConst()) => FalseConst()
          case (FalseConst(), _) => FalseConst()
          case (expr, TrueConst()) => expr
          case (TrueConst(), expr) => expr
          case (exprL, exprR) if exprL == exprR => exprL
          case (exprL, exprR) =>
            if (exprL != left || exprR != right) Simplifier(BinExpr("and", exprL, exprR))
            else BinExpr("and", exprL, exprR)
        }

      case ("or", left, right) =>
        val sLeft = Simplifier(left)
        val sRight = Simplifier(right)
        (sLeft, sRight) match {
          case (_, TrueConst()) => TrueConst()
          case (TrueConst(), _) => TrueConst()
          case (expr, FalseConst()) => expr
          case (FalseConst(), expr) => expr
          case (exprL, exprR) if exprL == exprR => exprL
          case (exprL, exprR) =>
            if (exprL != left || exprR != right) Simplifier(BinExpr("or", exprL, exprR))
            else BinExpr("or", exprL, exprR)
        }

      case (_, _,_) => expr

    }

  }

}
