
def getX: Int = {
  println("- getX output -")
  1
}

def callByValue(i: Int):Unit = {
  println(i+1)
  println(i+2)
  println(i+3)
}

callByValue(getX)

def callByName(i: => Int):Unit = {
  println(i+1)
  println(i+2)
  println(i+3)
}

callByName(getX)

//------------------------------------------------
def getY(inp: Int): Int = {
  println("- getY output -")
  inp*10
}

def getY2(): Int = {
  println("- getY2 output -")
  10
}

//val f : Int => Int = ???
/*
def callByValueY(func: Int => Int): Unit = {
  println(func(1)+1)
  println(func(2)+2)
  println(func(3)+3)
}

callByValueY(getY)*/

def callByValueY2(func: () => Int): Unit = {
  println(func()+1)
  println(func()+2)
  println(func()+3)
}

callByValueY2(getY2)

