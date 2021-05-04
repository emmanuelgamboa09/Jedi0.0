package value

import context._
import expression.Literal

case class Boole(value: Boolean) extends Literal {

  def &&(other: Value): Boole = {
    other match {
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Must be a boole value")
    }
  }

  def ||(other: Value): Boole = {
    other match {
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Must be a boole value")
    }
  }

  def unary_! : Boole = Boole(!this.value)

  override def equals(obj: Any): Boolean = {
    obj match {
      case x: Boole => x.value == this.value
      case _ => false
    }
  }

  override def toString: String = value.toString

  override def hashCode(): Int = this.toString.##
}

object Boole {
  val FALSE: Boole = Boole(false)
  val TRUE: Boole = Boole(true)
}




