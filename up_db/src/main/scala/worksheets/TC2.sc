case class Person(name: String, age: Int)

//----------------------------------------------------------
  //Step 1: The type class
  trait Printer[A] {
    def printProperty(a: A): Unit
  }

  //Step 2: Type class instances
  object PrinterInstances{
    implicit val personPrinter = new Printer[Person] {
      def printProperty(p: Person): Unit = {
        println(s"${p.name} with age = ${p.age}")
      }
    }
  }

  //Step 3: The API (interface)
  object PrinterSyntax {
    implicit class PrinterOps[Person](value: Person) {
      def printProperty(implicit instance: Printer[Person]): Unit = {
        instance.printProperty(value)
      }
    }
  }

import PrinterInstances.personPrinter
import PrinterSyntax.PrinterOps

Person("John",30).printProperty







