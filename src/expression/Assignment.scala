package expression

import context.{Environment, TypeException}
import value.{Notification, Value, Variable}

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    if (!vbl.execute(env).isInstanceOf[Variable]) {
      throw new TypeException("You can only assign variables")
    }
    vbl.execute(env).asInstanceOf[Variable].content = update.execute(env)
    Notification.DONE
  }
}
