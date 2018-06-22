package DigitalCircuitSimulation

/**
  * https://www.elprocus.com/half-adder-and-full-adder/
  */
abstract class Circuits extends Gates {
  // Sum = a|b & ¬(a&b)  Carry = a&b
  def halfAdder(a: Wire, b: Wire, sum: Wire, c: Wire): Unit = {
    val d, e = new Wire

    andGate(a, b, c)

    orGate(a, b, d)
    inverter(c, e)
    andGate(d, e, sum)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire

    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)

    orGate(c1, c2, cout)
  }

}
