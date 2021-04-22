package expression
import context.{Environment, TypeException, UndefinedException, alu}
import value.{Closure, Value}
case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {
    val arguments = operands.map(_.execute(env))
    try{
      val tempOperator = operator.execute(env)
      if(!tempOperator.isInstanceOf[Closure]) throw new TypeException("Not a closure")
      tempOperator.asInstanceOf[Closure](arguments)
    }catch{
      case _: UndefinedException => alu.execute(operator, arguments)
    }
  }
}
