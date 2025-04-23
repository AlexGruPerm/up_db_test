
object Types{
  type funcType[A] = (A,A) => Boolean
}

class Container {
  def isSorted1[A](a: Array[A], sortFunc: (A,A) => Boolean): Boolean = {
    val l: Int = a.length
    a.drop(1).zip(a.take(l-1)).forall(pair => sortFunc(pair._1,pair._2))
  }

  def isSorted2[A](a: Array[A], sortFunc: (A,A) => Boolean): Boolean = {
    Range(0,a.length-1).forall(indices => sortFunc(a(indices+1),a(indices)))
  }
}

object Application extends App {
  import Types._

  override def main(args: Array[String]): Unit ={
    println("Begin application")
    val sf1: funcType[Int] = _ > _

    val arr: Array[Int] = (1 to Int.MaxValue/200).toArray

    val cont = new Container()

    cont.isSorted1(arr, sf1)
    cont.isSorted2(arr, sf1)
    println("End application")
  }

}
