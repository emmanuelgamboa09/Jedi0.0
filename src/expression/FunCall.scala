package expression
import context.Environment
import value.Value
import context.alu
case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {
    val arguments = operands.map(_.execute(env))
    alu.execute(operator, arguments)
  }
}
