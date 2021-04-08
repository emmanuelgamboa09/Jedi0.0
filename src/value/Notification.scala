package value

case class Notification(notification: String) extends Value{
  override def toString: String = notification
}

object Notification{
  val OK: Notification = Notification("OK")
  val DONE: Notification = Notification("DONE")
  val UNSPECIFIED: Notification = Notification("UNSPECIFIED")
}