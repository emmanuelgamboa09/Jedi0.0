package value

import context.Environment
import expression.Expression

class Thunk(thunkBody: Expression, thunkEnv: Environment) extends Closure(Nil, thunkBody, thunkEnv) {
  private var cache: Value = _

  def apply(): Value = {
    if (cache == null) {
      cache = super.apply(Nil)
    }
    cache
  }
}

object Thunk {
  def apply(thunkBody: Expression, thunkEnv: Environment) = new Thunk(thunkBody, thunkEnv)
}
