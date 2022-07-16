package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map{strExprpair =>
      (strExprpair._1, Signal.Var{
        eval(getReferenceExpr(strExprpair._1, namedExpressions), namedExpressions)
        })
    }
    

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    if references.isEmpty then
      Double.NaN
    else
      expr match
        case Literal(x) => x
        case Ref(x) => eval(getReferenceExpr(x, references.tail), references)
        case Plus(x,y) => eval(x, references) + eval(y, references)
        case Minus(x,y) => eval(x, references) - eval(y, references)
        case Times(x,y) => eval(x, references) * eval(y, references)
        case Divide(x,y) => eval(x, references) / eval(y, references)

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
