
case class Animal(kind: String, nickname: String)

trait Printer[A]{
  def printInfo(a: A)
}

object PrinterInst{
  implicit val printerAnimal = new Printer[Animal]{
    def printInfo(a: Animal) =
      println(s"This is a ${a.kind} with name ${a.nickname}")
  }
}

object PrinterSyntax{
  implicit class PrinterOps[Animal](value: Animal) {
    def printInfo(implicit instance: Printer[Animal]) =
      instance.printInfo(value)
  }
}

import PrinterInst.printerAnimal
import PrinterSyntax.PrinterOps

val lion = Animal("Lion","John")
lion.printInfo