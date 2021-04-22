package value

import context.Environment
import expression.{Expression, Identifier}

case class Closure(val parameters: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    val tempExtension = new Environment(defEnv)
    tempExtension.bulkPut(parameters, args)
    Some(tempExtension).map(body.execute).get
  }
}
