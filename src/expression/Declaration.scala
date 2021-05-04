package expression

import context.Environment
import value._

case class Declaration(identifier: Identifier, exp: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    env(identifier) = exp.execute(env)
    Notification.OK
  }
}
