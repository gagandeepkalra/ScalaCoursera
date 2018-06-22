package DigitalCircuitSimulation

abstract class Gates extends Simulation {

  def InverterDelay: Int

  def AndGateDelay: Int

  def OrGateDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSigValue: Boolean = sigVal

    def setSigValue(s: Boolean): Unit = if (s != sigVal) {
      sigVal = s
      actions foreach (_())
    }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val sigVal: Boolean = input.getSigValue
      afterDelay(InverterDelay) {
        output setSigValue !sigVal
      }
    }

    input addAction invertAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val in1Sig = in1.getSigValue
      val in2Sig = in2.getSigValue
      afterDelay(AndGateDelay) {
        output setSigValue in1Sig & in2Sig
      }
    }

    in1 addAction andAction
    in2 addAction andAction
  }

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSigValue
      val in2Sig = in2.getSigValue
      afterDelay(AndGateDelay) {
        output setSigValue in1Sig || in2Sig
      }
    }

    in1 addAction orAction
    in2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime new-value = ${wire.getSigValue}")
    }

    wire addAction probeAction
  }

}
