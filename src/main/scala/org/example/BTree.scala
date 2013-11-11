package org.example

case class BTree(node: Node) {
  def size: Int = node.size

  def max: Int = node.max

  def min: Int = node.min

  def sum: Int = node.sum

  def avg: Int = node.sum / node.size

  def find(n: Int): Option[Node] = node.find(n)
}

trait Node {
  def max: Int

  def min: Int

  def size: Int

  def sum: Int
  def find(n:Int): Option[Node]
}

case class Branch(left: Node, mid: Int, right: Node) extends Node {
  require(left.max <= mid)
  require(mid <= right.min)

  def max: Int = right.max

  def min: Int = left.min

  def size: Int = right.size + 1 + left.size

  def sum: Int = right.sum + mid + left.sum
  def find(n:Int): Option[Node] = n match {
    case _ if n == mid => Some(this)
    case _ if n > mid => right.find(n)
    case _ if n < mid => left.find(n)
  }
}

case class Leaf(mid: Int) extends Node {
  def max: Int = mid

  def min: Int = mid

  def size: Int = 1

  def sum: Int = mid
  def find(n:Int): Option[Node] = n compare mid match {
    case 0 => Some(this)
    case _ => None
  }
}

object BTree {
  def from(list:List[Int]): Branch = {
    val mid: Int = list.length / 2
    val (left, dummyRight) = list.splitAt(mid)
    val (dummyLeft, right) = list.splitAt(mid + 1)
    list.length match {
      case 3 => Branch(Leaf(list(0)), list(1), Leaf(list(2)))
      case _ => Branch(from(left), list(mid),  from(right))
    }
  }
}
