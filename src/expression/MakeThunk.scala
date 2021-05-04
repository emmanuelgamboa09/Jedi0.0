package expression

import context.Environment
import value.{Thunk, Value}

case class MakeThunk(body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    Thunk(body, env)
  }
}
