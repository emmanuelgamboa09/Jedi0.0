package expression
import context.Environment
import value.Value

case class Block(val expression: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)
    expression.map(_.execute(tempEnv)).last
  }
}
