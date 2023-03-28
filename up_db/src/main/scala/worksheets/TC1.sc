
trait LivingObject
case class Person(name: String, age: Int) extends LivingObject
case class Animal(nickname: String) extends LivingObject

//The combination of types parameterized and implicit parameters
// is also called type classes.
/*
Type classes consist of three components:

The type class, which is defined as a trait that takes at least one generic parameter
(a generic “type”)
Instances of the type class for types you want to extend
Interface methods you expose to users of your new APIType classes consist of three components:

The type class, which is defined as a trait that takes at least one generic parameter
(a generic “type”)
Instances of the type class for types you want to extend
Interface methods you expose to users of your new API
*/
//----------------------------------------------------------
  //Step 1: The type class
  trait Printer[A] {
    def printProperty(a: A): Unit
  }

  //Step 2: Type class instances
  object PrinterInstances{

    //only for Person
    implicit val personPrinter = new Printer[Person] {
      def printProperty(p: Person): Unit = {
        println(s"${p.name} with age = ${p.age}")
      }
    }
    //only for Animal
    implicit val animalPrinter = new Printer[Animal] {
      println("Instance animalPrinter created. ")
      def printProperty(a: Animal): Unit = {
        println(s"ANM(${a.nickname})")
      }
    }

    //for for LivingObject
    implicit val livobjPrinter = new Printer[LivingObject] {
      def printProperty(a: LivingObject): Unit = {
        a match {
          case p: Person => personPrinter.printProperty(p)
          case a: Animal => animalPrinter.printProperty(a)
          case _ => println("unknown")
        }
      }
    }

  }

  //Step 3: The API (interface)
  object PrinterSyntax {
    implicit class PrinterOps[A](value: A) {
      def printProperty(implicit PrinterInstances: Printer[A]): Unit = {
        PrinterInstances.printProperty(value)
      }
    }
  }
//----------------------------------------------------------
import PrinterInstances.personPrinter
//import PrinterInstances.animalPrinter
//import PrinterInstances.livobjPrinter
import PrinterSyntax.PrinterOps

val persons = List(Person("John",30), Person("Ann",20))
//val animals = List(Animal("Lion"),Animal("Wolf"))

persons.foreach(p => p.printProperty)
//animals.foreach(a => a.printProperty)
/*
val objs: List[LivingObject] = persons ++ animals
objs.foreach(a => a.printProperty)*/





