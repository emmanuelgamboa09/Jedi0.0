package expression
import context.{Environment, UndefinedException}
import value.Value

case class Identifier(name: String) extends Expression {
  override def toString: String = name
  // def execute(env: Environment) = ???
  override def execute(env: Environment): Value = {
    env match{
      case value: Value => value
      case _ => throw new UndefinedException(this)
    }
  }
}
