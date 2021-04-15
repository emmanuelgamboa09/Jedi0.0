package expression
import context.{Environment, TypeException}
import value.Boole.{FALSE, TRUE}
import value.{Boole, Value}

case class Disjunction(operand: List[Expression]) extends SpecialForm{
  override def execute(env: Environment): Value = {
    try{
      operand.foreach(o => if(o.execute(env).asInstanceOf[Boole].value) return TRUE)
      FALSE
    }catch{
      case _: TypeException => throw new TypeException("Must be a boole value")
    }
  }
}
