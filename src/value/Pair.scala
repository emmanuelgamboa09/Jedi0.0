package value

case class Pair(var first: Value = null, var second: Value = null) extends Value with Ordered[Value] {
  override def toString: String = "(" + first + ", " + second + ")"

  override def compare(that: Value): Int = this.toString.compareTo(that.toString)
}
