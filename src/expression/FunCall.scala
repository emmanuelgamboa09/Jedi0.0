package expression
import context._
import value.{Closure, Value}
case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {
    var args: List[Value] = Nil
    if(env.contains(operator)){
      if (flags.paramPassing == flags.BY_NAME) {
        args = operands.map(MakeThunk(_).execute(env))
      } else {
        args = operands.map(_.execute(env))
      }
      try {
        val tempOperator = operator.execute(env)
        if (!tempOperator.isInstanceOf[Closure]) throw new TypeException("Not a closure")
        tempOperator.asInstanceOf[Closure](args)
      } catch {
        case _: UndefinedException => alu.execute(operator, args)
      }
    }else{
      args = operands.map(_.execute(env))
      alu.execute(operator, args)
    }
  }
}
