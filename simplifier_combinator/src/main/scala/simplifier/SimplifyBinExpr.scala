package simplifier
import AST._
import math.pow


object SimplifyBinExpr {
  def apply(binExpression: BinExpr): Node = {
    (binExpression.op, binExpression.left, binExpression.right) match {
      case ("+", Tuple(l1), Tuple(l2)) => Tuple((l1 ++ l2) map Simplifier.simplify)
      case ("+", ElemList(l1), ElemList(l2)) => ElemList((l1 ++ l2) map Simplifier.simplify)
      case (op, IntNum(r1), IntNum(r2)) =>
        op match {
          case "-" => IntNum(r1 - r2)
          case "+" => IntNum(r1 + r2)
          case "/" => IntNum(r1 / r2)
          case "*" => IntNum(r1 * r2)
          case "%" => IntNum(r1 % r2)
          case "**" => IntNum(pow(r1.toDouble, r2.toDouble).toInt)

          case "==" => if (r1 == r2) TrueConst() else FalseConst()
          case "!=" => if (r1 != r2) TrueConst() else FalseConst()
          case ">=" => if (r1 >= r2) TrueConst() else FalseConst()
          case "<=" => if (r1 <= r2) TrueConst() else FalseConst()
          case ">" => if (r1 > r2)   TrueConst() else FalseConst()
          case "<" => if (r1 < r2)   TrueConst() else FalseConst()
        }

      case (op, FloatNum(r1), FloatNum(r2)) =>
        op match {
          case "-" => FloatNum(r1 - r2)
          case "+" => FloatNum(r1 + r2)
          case "/" => FloatNum(r1 / r2)
          case "*" => FloatNum(r1 * r2)
          case "%" => FloatNum(r1 % r2)
          case "**" => FloatNum(pow(r1, r2))

          case "==" => if (r1 == r2) TrueConst() else FalseConst()
          case "!=" => if (r1 != r2) TrueConst() else FalseConst()
          case ">=" => if (r1 >= r2) TrueConst() else FalseConst()
          case "<=" => if (r1 <= r2) TrueConst() else FalseConst()
          case ">" => if (r1 > r2)  TrueConst() else FalseConst()
          case "<" => if (r1 < r2)  TrueConst() else FalseConst()
        }

      case (op, IntNum(r1), FloatNum(r2)) =>
        op match {
          case "+" => FloatNum(r1 + r2)
          case "-" => FloatNum(r1 - r2)
          case "*" => FloatNum(r1 * r2)
          case "/" => FloatNum(r1 / r2)
          case "%" => FloatNum(r1 % r2)
          case "**" => FloatNum(pow(r1.toDouble, r2))

          case "==" => if (r1 == r2) TrueConst() else FalseConst()
          case "!=" => if (r1 != r2) TrueConst() else FalseConst()
          case ">=" => if (r1 >= r2) TrueConst() else FalseConst()
          case "<=" => if (r1 <= r2) TrueConst() else FalseConst()
          case ">" => if (r1 > r2)  TrueConst() else FalseConst()
          case "<" => if (r1 < r2)  TrueConst() else FalseConst()
        }

      case (op, FloatNum(r1), IntNum(r2)) =>
        op match {
          case "+" => FloatNum(r1 + r2)
          case "-" => FloatNum(r1 - r2)
          case "*" => FloatNum(r1 * r2)
          case "/" => FloatNum(r1 / r2)
          case "%" => FloatNum(r1 % r2)
          case "**" => FloatNum(pow(r1, r2.toDouble))

          case "==" => if (r1 == r2) TrueConst() else FalseConst()
          case "!=" => if (r1 != r2) TrueConst() else FalseConst()
          case ">=" => if (r1 >= r2) TrueConst() else FalseConst()
          case "<=" => if (r1 <= r2) TrueConst() else FalseConst()
          case ">"  => if (r1 > r2)  TrueConst() else FalseConst()
          case "<"  => if (r1 < r2)  TrueConst() else FalseConst()
        }

      case ("==", r1, r2) if r1 == r2 => TrueConst()
      case (">=", r1, r2) if r1 == r2 => TrueConst()
      case ("<=", r1, r2) if r1 == r2 => TrueConst()
      case ("!=", r1, r2) if r1 == r2 => FalseConst()
      case ("<", r1, r2) if r1 == r2 => FalseConst()
      case (">", r1, r2) if r1 == r2 => FalseConst()
      case ("or", r1, r2) if r1 == r2 => r1
      case ("and", r1, r2) if r1 == r2 => r1

      //(((x1 * y1 + x2 * y2) + x3 * y3) + (x4 * y4)) => (x1 * y1 + x2 * y2) + (x3 * y3 + x4 * y4)
      case ("+", BinExpr("+", BinExpr("+", BinExpr("*", x1, y1), BinExpr("*", x2, y2)), BinExpr("*", x3, y3)), BinExpr("*", x4, y4))
        => Simplifier(BinExpr("+", BinExpr("+", BinExpr("*", x1, y1), BinExpr("*", x2, y2)), BinExpr("+", BinExpr("*", x3, y3), BinExpr("*", x4, y4))))


      case ("-", left, right)    => (Simplifier(left), Simplifier(right)) match {
        case (binExpressionLeft, binExpressionRight) if binExpressionLeft == binExpressionRight => IntNum(0)
        case (binExpression, IntNum(n)) if n == 0  => binExpression
        case (binExpression, FloatNum(n)) if n == 0 => binExpression
        case (IntNum(n), binExpression) if n == 0  => Simplifier(Unary("-", binExpression))
        case (FloatNum(n), binExpression) if n == 0 => Simplifier(Unary("-", binExpression))

        // właściwości rozdzielcze ("*")
        //parseString("2*x-x") mustEqual parseString("x")
        case (BinExpr("*", l, r), binExpression) if binExpression == l => Simplifier(BinExpr("*", BinExpr("-", r, IntNum(1)), l))
        case (BinExpr("*", l, r), binExpression) if binExpression == r => Simplifier(BinExpr("*", BinExpr("-", l, IntNum(1)), r))

        // właściwości rozdzielcze dzielenia ("/")
        case (e1@BinExpr("/", l1, r1), e2@BinExpr("/", l2, r2)) =>
          if (r1 == r2) BinExpr("/", BinExpr("-", l1, l2), r1)
          else {
            val s1 = Simplifier(e1)
            val s2 = Simplifier(e2)
            if (s1 != e1 || s2 != e2) Simplifier(BinExpr("-", s1, s2)) else BinExpr("-", s1, s2)
          }


        //parseString("(x+y)**2-(x-y)**2") mustEqual parseString("4*x*y")
        case (e1@BinExpr("**", BinExpr(op1, x1, y1), IntNum(a)), e2@BinExpr("**", BinExpr(op2, x2, y2), IntNum(b)))
          if x1 == x2 && y1 == y2 && a == 2 && b == 2 =>
            if (op1 == "-" && op2 == "+") Unary("-", BinExpr("*", BinExpr("*", x1, IntNum(4)), y1))
            else if (op1 == "+" && op2 == "-") BinExpr("*", BinExpr("*", x1, IntNum(4)), y1)
            else BinExpr("-", Simplifier(e1), Simplifier(e2))

        //parseString("(x+y)**2-x**2-2*x*y") mustEqual parseString("y**2")
        case (BinExpr("-", BinExpr("**", BinExpr("+", x1, y1), IntNum(a)), BinExpr("**", x2, IntNum(b))), BinExpr("*", BinExpr("*", x3, IntNum(c)), y3))
          if a == 2 && b == 2 && c == 2 && x1 == x2 && ((x1 == x3 && y1 == y3) || (x1 == y3 && y1 == x3)) =>
            Simplifier(BinExpr("**", y1, IntNum(2)))

        // właściwości przemienne
        // parseString("x+5-x") mustEqual parseString("5")
        case (BinExpr("+", binExpressionLeft, binExpressionRight), binExpressionLeft1) => Simplifier(binExpressionRight)

        //TODO check once again
        case (binExpressionLeft, binExpressionRight) =>
          val sL = Simplifier(binExpressionLeft)
          val sR = Simplifier(binExpressionRight)
          if (sL != binExpressionLeft || sR != binExpressionRight) Simplifier(BinExpr("-", sL, sR)) else BinExpr("-", sL, sR)
      }

      case ("+", left, right) => (Simplifier(left), Simplifier(right)) match {
        case (binExpression, IntNum(n)) if n == 0  => binExpression
        case (IntNum(n), binExpression) if n == 0  => binExpression
        case (binExpression, FloatNum(n)) if n == 0 => binExpression
        case (FloatNum(n), binExpression) if n == 0 => binExpression
        case (Unary("-", binExpressionU), binExpression) => Simplifier(BinExpr("-", binExpression, binExpressionU))
        case (binExpression, Unary("-", binExpressionU)) => Simplifier(BinExpr("-", binExpression, binExpressionU))

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

        case (binExpressionLeft, binExpressionRight)      =>
          val sL = Simplifier(binExpressionLeft)
          val sR = Simplifier(binExpressionRight)
          if (sL != binExpressionLeft || sR != binExpressionRight) Simplifier(BinExpr("+", sL, sR)) else BinExpr("+", sL, sR)
      }

      case ("*", left, right)    => (left, right) match {
        case (binExpression, IntNum(n))   => if (n == 1) Simplifier(binExpression) else if (n == 0) IntNum(0) else {
          val s = Simplifier(binExpression)
          if (s != binExpression) Simplifier(BinExpr("*", s, IntNum(n))) else BinExpr("*", s, IntNum(n))
        }
        case (IntNum(n), binExpression)   => if (n == 1) Simplifier(binExpression) else if (n == 0) IntNum(0)   else {
          val s = Simplifier(binExpression)
          if (s != binExpression) Simplifier(BinExpr("*", s, IntNum(n))) else BinExpr("*", s, IntNum(n))
        }
        case (binExpression, FloatNum(n)) => if (n == 1) Simplifier(binExpression) else if (n == 0) FloatNum(0) else {
          val s = Simplifier(binExpression)
          if (s != binExpression) Simplifier(BinExpr("*", s, FloatNum(n))) else BinExpr("*", s, FloatNum(n))
        }
        case (FloatNum(n), binExpression) => if (n == 1) Simplifier(binExpression) else if (n == 0) FloatNum(0) else {
          val s = Simplifier(binExpression)
          if (s != binExpression) Simplifier(BinExpr("*", s, FloatNum(n))) else BinExpr("*", s, FloatNum(n))
        }

        // power laws:
        case (BinExpr("**", leftL, rightL), BinExpr("**", leftR, rightR)) if leftL == leftR =>
          Simplifier(BinExpr("**", leftL, BinExpr("+", rightL, rightR))) // TODO: czy potrzebny wewnetrzny Simplifier?

        case (binExpression, BinExpr("/", binExpressionNum, binExpressionDenom)) => Simplifier(BinExpr("/", BinExpr("*", binExpression, binExpressionNum), binExpressionDenom))
        // case (BinExpr("/", binExpressionNum, binExpressionDenom), binExpression) => Simplifier(BinExpr("/", BinExpr("*", binExpression, binExpressionNum), binExpressionDenom))
        case (binExpressionLeft, binExpressionRight)      =>
          val sL = Simplifier(binExpressionLeft)
          val sR = Simplifier(binExpressionRight)
          if (sL != binExpressionLeft || sR != binExpressionRight) Simplifier(BinExpr("*", sL, sR)) else BinExpr("*", sL, sR)
      }

      case ("/", left, right) => (left, right) match {
        case (binExpressionLeft, binExpressionRight) if binExpressionLeft == binExpressionRight => IntNum(1)
        // nazwy ponizej moga nie byc najbardziej czytelne, ale to raczej przez specyfike problemu :)
        case (binExpressionNum, BinExpr("/", denomNum, denomDenom)) =>
          val sNum = Simplifier(binExpressionNum)
          val sDenomNum = Simplifier(denomNum)
          val sDenomDenom = Simplifier(denomDenom)
          sNum match {
            case BinExpr("/", sNumNum, sNumDenom) =>
              Simplifier(BinExpr("/", BinExpr("*", sNumNum, sDenomDenom), BinExpr("*", sNumDenom, sDenomNum)))
            case binExpression => Simplifier(BinExpr("/", BinExpr("*", binExpression, sDenomDenom), sDenomNum))
          }

        // power laws:
        case (BinExpr("**", leftL, rightL), BinExpr("**", leftR, rightR)) if leftL == leftR =>
          Simplifier(BinExpr("**", leftL, BinExpr("-", rightL, rightR))) // TODO: czy potrzebny wewnetrzny Simplifier?

        case (binExpression, IntNum(n))   if n == 1 => Simplifier(binExpression)
        case (binExpression, FloatNum(n)) if n == 1 => Simplifier(binExpression)
        case (binExpressionLeft, binExpressionRight) =>
          val sL = Simplifier(binExpressionLeft)
          val sR = Simplifier(binExpressionRight)
          if (sL != binExpressionLeft || sR != binExpressionRight) Simplifier(BinExpr("/", sL, sR)) else BinExpr("/", sL, sR)
      }

      case ("**", left, right)    => (Simplifier(left), Simplifier(right)) match {
        case (binExpression, IntNum(n))   => if (n == 1) Simplifier(binExpression) else if (n == 0) IntNum(1) else {
          val s = Simplifier(binExpression)
          if (s != binExpression) Simplifier(BinExpr("**", s, IntNum(n))) else BinExpr("**", s, IntNum(n))
        }
        // power laws:
        case (BinExpr("**", l, r), binExpression) => Simplifier(BinExpr("**", l, BinExpr("*", r, binExpression)))

        case (binExpressionLeft, binExpressionRight)      =>
          val sL = Simplifier(binExpressionLeft)
          val sR = Simplifier(binExpressionRight)
          if (sL != binExpressionLeft || sR != binExpressionRight) Simplifier(BinExpr("**", sL, sR)) else BinExpr("**", sL, sR)
      }

      case ("and", left, right) =>
        val sLeft = Simplifier(left)
        val sRight = Simplifier(right)
        (sLeft, sRight) match {
          case (_, FalseConst()) => FalseConst()
          case (FalseConst(), _) => FalseConst()
          case (binExpression, TrueConst()) => binExpression
          case (TrueConst(), binExpression) => binExpression
          case (binExpressionLeft, binExpressionRight) if binExpressionLeft == binExpressionRight => binExpressionLeft
          case (binExpressionLeft, binExpressionRight) =>
            if (binExpressionLeft != left || binExpressionRight != right) Simplifier(BinExpr("and", binExpressionLeft, binExpressionRight))
            else BinExpr("and", binExpressionLeft, binExpressionRight)
        }

      case ("or", left, right) =>
        val sLeft = Simplifier(left)
        val sRight = Simplifier(right)
        (sLeft, sRight) match {
          case (_, TrueConst()) => TrueConst()
          case (TrueConst(), _) => TrueConst()
          case (binExpression, FalseConst()) => binExpression
          case (FalseConst(), binExpression) => binExpression
          case (binExpressionLeft, binExpressionRight) if binExpressionLeft == binExpressionRight => binExpressionLeft
          case (binExpressionLeft, binExpressionRight) =>
            if (binExpressionLeft != left || binExpressionRight != right) Simplifier(BinExpr("or", binExpressionLeft, binExpressionRight))
            else BinExpr("or", binExpressionLeft, binExpressionRight)
        }

      case (_, _,_) => binExpression

    }

  }

}
