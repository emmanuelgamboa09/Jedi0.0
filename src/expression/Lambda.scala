package expression

import context.Environment
import value.{Closure, Value}

case class Lambda(val paramaters: List[Identifier], val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    Closure(paramaters, body, env)
  }
}
