package value

case class Variable(var content: Value) extends Value with Ordered[Value] {
  override def toString: String = "[" + content + "]"

  override def compare(that: Value): Int = this.compareTo(that)
}
