package com.jps.wbtree

import com.jps.wbtree.DataOrdering.MyOrdering

object DataOrdering {
  trait MyOrdering[T] {
    def lt(a: T, b: T): Boolean
    def eq(a: T, b: T): Boolean
    def gt(a: T, b: T): Boolean
  }
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
  implicit object NothingOrdering extends MyOrdering[Nothing] {
    override def eq(a: Nothing, b: Nothing): Boolean = false
    override def lt(a: Nothing, b: Nothing): Boolean = false
    override def gt(a: Nothing, b: Nothing): Boolean = false
  }
}

private[wbtree] sealed abstract class WBTree[+A: MyOrdering] {
  def weight: Int
  def iterator[T >: A : MyOrdering]: Iterator[T] = Iterator.empty
  def add[T >: A : MyOrdering](valueAdded: T): WBTree[T] = Node(valueAdded, WBTreeNil, WBTreeNil)
  def exists[T >: A : MyOrdering](value: T): Boolean = false
  def rebalance[T >: A : MyOrdering]: WBTree[T] = this
  def singleLeftRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  def doubleLeftRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  def singleRightRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  def doubleRightRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
}

private case object WBTreeNil extends WBTree[Nothing] {
  def weight = 0
}

private case class Node[A: MyOrdering](value: A, left: WBTree[A], right: WBTree[A]) extends WBTree[A] {

  val delta: Double = 1 + Math.sqrt(2)
  val gamma: Double = Math.sqrt(2)

  override val weight: Int = left.weight + right.weight + 1

    override def add[T >: A : MyOrdering](valueAdded: T): WBTree[T] = {
      val comparator = implicitly[MyOrdering[T]]

      if (comparator.lt(valueAdded, value))
        Node[T](value, left.add(valueAdded), right).rebalance
      else
        Node(value, left, right.add(valueAdded)).rebalance
    }

    def remove[T >: A : MyOrdering](valueRemoved: T): WBTree[T] = {
      val comparator = implicitly[MyOrdering[T]]

      //decrementWeight
      //TODO
      this
    }
    override def rebalance[T >: A : MyOrdering]: WBTree[T] = {
      if (left.weight + right.weight < 2)
        this
      else if (right.weight > delta * left.weight)
        right match {
          case Node(_, rightLeft, rightRight) =>
            if (rightLeft.weight < rightRight.weight * gamma)
              singleLeftRotate
            else
              doubleLeftRotate
          case _ => throw new IllegalStateException()
        }
      else if (left.weight > delta * right.weight)
        left match {
          case Node(_, leftLeft, leftRight) =>
            if (leftRight.weight < leftLeft.weight * gamma)
              singleRightRotate
            else
              doubleRightRotate
          case _ => throw new IllegalStateException()
        }
      else
        this
    }
    override def singleLeftRotate[T >: A : MyOrdering]: Node[T] = {
      this match {
        case Node(curVal, curLeft, Node(curRightVal, curRightLeft, curRightRight))
          => Node(curRightVal, Node(curVal, curLeft, curRightLeft), curRightRight)
        case _ => throw new IllegalStateException()
      }
    }
    override def doubleLeftRotate[T >: A : MyOrdering]: Node[T] = {
      this match {
        case Node(curVal, curLeft, Node(rightVal, Node(rightLeftVal, rightLeftLeft, rightLeftRight), rightRight))
          => Node[T](rightLeftVal, Node(curVal, curLeft, rightLeftLeft), Node(rightVal, rightLeftRight, rightRight))
        case _ => throw new IllegalStateException()
      }
    }
    override def singleRightRotate[T >: A : MyOrdering]: Node[T] = {
      this match {
        case Node(curVal, Node(curLeftVal, curLeftLeft, curLeftRight), curRight)
        => Node(curLeftVal, curLeftLeft, Node(curVal, curLeftRight, curRight))
        case _ => throw new IllegalStateException()
      }
    }
    override def doubleRightRotate[T >: A : MyOrdering]: Node[T] = {
      this match {
        case Node(curVal, Node(leftVal, leftLeft, Node(leftRightVal, leftRightLeft, leftRightRight)), curRight)
          => Node(leftRightVal, Node(leftVal, leftLeft, leftRightLeft), Node(curVal, leftRightRight, curRight))
        case _ => throw new IllegalStateException()
      }
    }
    override def exists[T >: A : MyOrdering](valueExist: T): Boolean = {
      val comparator = implicitly[MyOrdering[T]]

      if (comparator.eq(valueExist, value))
        true
      else if (comparator.lt(valueExist, value))
        left.exists(valueExist)
      else
        right.exists(valueExist)
    }

}