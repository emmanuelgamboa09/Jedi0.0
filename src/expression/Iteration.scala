package expression

import context.{Environment, JediException}
import value.Boole.TRUE
import value.{Boole, Notification, Value}

case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    if (!condition.execute(env).isInstanceOf[Boole]) {
      throw new JediException("The condition must be a Boole value")
    }
    while (condition.execute(env) == TRUE) {
      body.execute(env)
    }
    Notification.DONE
  }
}
