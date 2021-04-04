package expression

import context.Environment
import value.Value

trait Literal extends Expression with Value{
  override def execute(env: Environment): Value = this
}

