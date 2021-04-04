package value
import context._
import expression.Literal

case class Boole(value: Boolean) extends Literal{
  
  def &&(other: Value): Boole = {
    other match {
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Must be a boole value")
    }
  }

  def ||(other: Value): Boole = {
    other match{
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Must be a boole value")
    }
  }

  def unary_! : Boole = Boole(!this.value)

  override def toString: String = value.toString

  override def hashCode(): Int = this.toString.##

  override def execute(env: Environment): Value = ???
}

object Boole{
  def FALSE: Boole = Boole(false)
  def TRUE: Boole = Boole(true)
}




