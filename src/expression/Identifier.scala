package expression
import context.Environment
import value.Value

case class Identifier(val name: String) extends Expression {
  override def toString: String = name
  // def execute(env: Environment) = ???
  override def execute(env: Environment): Value = ???
}
