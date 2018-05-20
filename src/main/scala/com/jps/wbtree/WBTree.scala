package com.jps.wbtree

object DataOrdering {
  implicit object DoubleOrdering extends MyOrdering[Double] {
    override def eq(a: Double, b: Double): Boolean = a == b
    override def lt(a: Double, b: Double): Boolean = a < b
    override def gt(a: Double, b: Double): Boolean = a > b
  }
  implicit object IntOrdering extends MyOrdering[Int] {
    override def eq(a: Int, b: Int): Boolean = a == b
    override def lt(a: Int, b: Int): Boolean = a < b
    override def gt(a: Int, b: Int): Boolean = a > b
  }
  implicit object StringOrdering extends MyOrdering[String] {
    override def eq(a: String, b: String): Boolean = a == b
    override def lt(a: String, b: String): Boolean = a.compareTo(b) < 0
    override def gt(a: String, b: String): Boolean = a.compareTo(b) > 0
  }
}

class WBTree[T: MyOrdering] {

  private var root: Option[Node] = None

  class Node(value: T, var weight: Int, var parent: Option[Node] = None,
             var left: Option[Node] = None, var right: Option[Node] = None) {

    def add(valueAdded: T): Unit = {
      val comparator = implicitly[MyOrdering[T]]

      incrementWeight
      if(comparator.lt(valueAdded, value))
        left match {
          case None => left = Some(new Node(valueAdded, 1, Some(this)))
          case Some(leftChild) => leftChild.add(valueAdded)
        }
      else
        right match {
          case None => right = Some(new Node(valueAdded, 1, Some(this)))
          case Some(rightChild) => rightChild.add(valueAdded)
        }
    }
    def remove(valueRemoved: T): Unit = {
      val comparator = implicitly[MyOrdering[T]]

      decrementWeight
      //TODO

    }
    def exists(valueExist: T): Boolean = {
      val comparator = implicitly[MyOrdering[T]]

      if(comparator.eq(valueExist, value))
        true
      else if(comparator.lt(valueExist, value))
        left match {
          case None => false
          case Some(leftChild) => leftChild.exists(valueExist)
        }
      else
        right match {
          case None => false
          case Some(rightChild) => rightChild.exists(valueExist)
        }

    }
    def incrementWeight(): Unit = {
      weight = weight + 1
    }
    def decrementWeight(): Unit = {
      weight = weight - 1
    }
  }

  def add(valueAdded: T): Unit = {
    if(!exists(valueAdded))
      root match {
        case None => {
          root = Some(new Node(valueAdded, 1))
        }
        case Some(rootNode) => {
          rootNode.add(valueAdded)
        }
      }

  }
  def remove(valueRemoved: T): Unit = {
    if(exists(valueRemoved))
      root.get.remove(valueRemoved)

  }
  def exists(valueExist: T): Boolean = {
    root match {
      case None => false
      case Some(rootNode) => rootNode.exists(valueExist)
    }
  }

}
