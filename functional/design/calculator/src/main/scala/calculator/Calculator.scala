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
    namedExpressions.map {
      case (key, value) => (key, Signal {
        eval(value(), namedExpressions)
      })
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    def helper(expr: Expr, references: Map[String, Signal[Expr]], explored: Set[String]): Double =
      expr match
        case Expr.Literal(v) => v
        case Expr.Ref(name) =>
          if explored.contains(name) then Double.NaN
          else helper(getReferenceExpr(name, references), references, explored.incl(name))
        case Expr.Plus(a, b) => helper(a, references, explored) + helper(b, references, explored)
        case Expr.Minus(a, b) => helper(a, references, explored) - helper(b, references, explored)
        case Expr.Times(a, b) => helper(a, references, explored) * helper(b, references, explored)
        case Expr.Divide(a, b) => helper(a, references, explored) / helper(b, references, explored)
    helper(expr, references, Set())

  /** Get the Expr for a referenced variables.
   * If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
