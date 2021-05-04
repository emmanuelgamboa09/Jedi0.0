package expression

import context.{Environment, UndefinedException}
import value.{Text, Thunk, Value}

case class Identifier(name: String) extends Expression {
  override def toString: String = name

  override def execute(env: Environment): Value = {
    env(this) match {
      case text: Text => text.body.execute(env)
      case thunk: Thunk => thunk.apply()
      case value: Value => value
      case _ => throw new UndefinedException(this)
    }
  }
}
