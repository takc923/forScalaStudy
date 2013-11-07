package org.example

object Sum extends App {
  def sum(l: List[Int]): Int = l reduceLeft(_+_)
}
