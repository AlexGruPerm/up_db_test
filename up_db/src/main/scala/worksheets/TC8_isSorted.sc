object Container{
  type funcType[A] = (A,A) => Boolean

  def isSorted1[A](a: Array[A], sortFunc: (A,A) => Boolean): Boolean = {
    val l: Int = a.length
    println("Begin application")
    a.drop(1).zip(a.take(l-1)).forall(pair => sortFunc(pair._1,pair._2))
  }

  def isSorted2[A](a: Array[A], sortFunc: (A,A) => Boolean): Boolean = {
    Range(0,a.length-1).forall(indices => sortFunc(a(indices+1),a(indices)))
  }

}

import Container._

val sf1: funcType[Int] = _ > _
val sf2: funcType[Int] = _ < _

val arr: Array[Int] = (1 to Int.MaxValue/100).toArray

isSorted1(arr, sf1)
isSorted2(arr, sf1)


