package value

case class Notification(notification: String) extends Value{
  override def toString: String = notification
}

object Notification{
  def OK: Notification = Notification("OK")
  def DONE: Notification = Notification("DONE")
  def UNSPECIFIED: Notification = Notification("UNSPECIFIED")
}
