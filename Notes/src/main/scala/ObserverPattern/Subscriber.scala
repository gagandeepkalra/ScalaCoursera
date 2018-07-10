package ObserverPattern

trait Subscriber {
  def handler(pub: Publisher)
}
