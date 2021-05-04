package expression

import context.{Environment, JediException, TypeException}
import value.Boole.{FALSE, TRUE}
import value.{Boole, Value}

case class Disjunction(operand: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    if (operand.size < 2)
      throw new JediException("Must have 2 or more operands inorder to perform this action")
    try {
      operand.foreach(o => if (o.execute(env).asInstanceOf[Boole].value) return TRUE)
      FALSE
    } catch {
      case _: TypeException => throw new TypeException("Must be a boole value")
    }
  }
}
