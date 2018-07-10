package ObserverPattern

trait Publisher {

  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit = subscribers += subscriber

  def unSubscribe(subscriber: Subscriber): Unit = subscribers -= subscriber

  def publish(): Unit = subscribers.foreach(_.handler(this))

}
