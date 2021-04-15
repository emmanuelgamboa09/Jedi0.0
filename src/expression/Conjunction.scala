package expression
import context.{Environment, JediException, TypeException}
import value.Boole.{FALSE, TRUE}
import value.{Boole, Value}

import scala.annotation.tailrec

case class Conjunction(operand: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    if(operand.size < 2)
      throw new JediException("Must have 2 or more operands inorder to perform this action")
    @tailrec
    def helper(list: List[Expression]): Boole = {
      if(list == Nil) TRUE
      else list.head.execute(env) match {
        case a: Boole =>
          if(a.value) helper(list.tail)
          else FALSE
        case _ => throw new TypeException("Conjunction requires Boole values")
      }
    }
    helper(operand)
  }
}
