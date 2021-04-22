package expression
import context.{Environment, UndefinedException}
import value.Value

case class Identifier(name: String) extends Expression {
  override def toString: String = name

  override def execute(env: Environment): Value = {
    env(this) match{
      case value: Value => value
      case _ => throw new UndefinedException(this)
    }
  }
}
