package org.example

object Factorial extends App {
  def fact(n: Int): Int = n match {
    case n: Int if n < 0 => -1
    case 0 | 1 => 1
    case _ => n * fact(n - 1)
  }

  def anotherFact(n:Int)= n match {
    case n: Int if n < 0 => -1
    case 0 => 1
    case _ => (1 to n).toList.reduceLeft(_*_)
  }
}
