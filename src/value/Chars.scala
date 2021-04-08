package value
import context.TypeException

case class Chars(value: String) extends Addable with Ordered[Value]{

  def size: Exact = Exact(value.length)

  override def +(other: Value): Chars = {
    other match {
      case x: Addable => Chars(this.value + x.toString)
      case _ => throw new TypeException("Must be an addable value")
    }
  }

  def subChars(exact1: Exact, exact2: Exact): Chars = {
    val upper = math.min(exact2.value, value.length)
    Chars(value.substring(exact1.value, upper))
  }

  override def equals(obj: Any): Boolean = {
    obj match{
      case x: Chars => x.value == this.value
      case _ => false
    }
  }

  override def compare(other: Value): Int = this.value.compare(other.toString)
  override def toString: String = value
  override def hashCode(): Int = this.toString.##
}