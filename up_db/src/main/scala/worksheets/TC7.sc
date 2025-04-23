class Cafe {
  def buyCoffee(cc: CreditCard, p: Payments): Coffee =

  val cup = new Coffee()
  p.charge(cc, cup.price)
  cup
}

class CreditCard()

trait Payments{
 def charge(cc: CreditCard, price: Double): Unit
}

class SimulatedPayments extends Payments {
  def charge(cc: CreditCard, price: Double): Unit =
    println("charging " + price + " to " + cc)
}

class Coffee{
val price: Double = 2.0
}

val cc = new CreditCard()
val p = new Payments()
val cafe = new Cafe()
val cup = cafe.buyCoffee(cc, p)