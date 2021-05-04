package expression

import context.Environment
import value.{Boole, Notification, Value}

case class Conditional(expression: Expression, condition: Expression, operand: Expression = null) extends SpecialForm {
  override def execute(env: Environment): Value = {
    if (expression.execute(env).asInstanceOf[Boole].value) condition.execute(env)
    else if (operand == null) Notification.UNSPECIFIED
    else operand.execute(env)
  }
}
