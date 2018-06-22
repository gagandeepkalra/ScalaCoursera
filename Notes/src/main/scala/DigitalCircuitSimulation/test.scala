package DigitalCircuitSimulation

object CircuitsTesting {


  def main(args: Array[String]): Unit = {

    object sim extends Circuits with Parameters
    import sim._

    val in1, in2, sum, carry = new Wire
    halfAdder(in1, in2, sum, carry)

    probe("sum", sum)
    probe("carry", carry)

    in1.setSigValue(true)
    run()

    in2.setSigValue(true)
    run()
  }


}
