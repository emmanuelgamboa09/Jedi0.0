package value

case object empty extends Value with Ordered[Value] {
  override def toString: String = "Nil"

  override def compare(that: Value): Int = this.compareTo(that)
}
