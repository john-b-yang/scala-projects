package mtscheme

// Case Classes

sealed trait ValueT
case class Num(v: BigDecimal) extends ValueT
case class Bool(v: Boolean) extends ValueT
case class Name(v: String) extends  ValueT

sealed trait ExprT
case class NullExpr() extends ExprT
case class Comb(v: List[ExprT]) extends ExprT
case class EList(v: List[ExprT]) extends ExprT
case class Func(args: List[ValueT], body: List[ExprT]) extends ExprT
case class Proc(f: ((Env, List[ExprT]) => (Env, ExprT))) extends ExprT

class Interpreter {
  
}
