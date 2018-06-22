package DigitalCircuitSimulation

trait Simulation {

  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event] // sorted by time

  private var agenda: Agenda = List()

  private var curTime = 0

  // public APIs

  def currentTime: Int = curTime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(curTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def run(): Unit = {
    afterDelay(0) {
      println(s"*** Simulation Started, Time = $curTime ***")
    }
    loop()
  }

  // private methods

  private def insert(ag: Agenda, item: Event): Agenda = {
    ag match {
      case first :: rest if item.time >= first.time => first :: insert(rest, item)
      case _ => item :: ag
    }
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    case Nil =>
  }
}
