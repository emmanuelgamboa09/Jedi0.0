package expression

import context.Environment
import value.{Text, Value}

case class MakeText(val body: Expression) extends Expression {
  override def execute(env: Environment): Value = {
    Text(body)
  }
}
