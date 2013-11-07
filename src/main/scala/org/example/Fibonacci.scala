package org.example

object Fibonacci extends App {
  def fib(n: Int): Int = n match {
    case n: Int if n < 0 => -1
    case 0 => 0
    case 1 | 2 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }
}
