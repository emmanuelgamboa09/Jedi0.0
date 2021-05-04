package expression

import context.Environment
import value.{Pair, Value}

case class MakePair(val first: Expression = null, val second: Expression = null) extends Expression {
  override def execute(env: Environment): Value = {
    Pair(first.execute(env), second.execute(env))
  }
}
