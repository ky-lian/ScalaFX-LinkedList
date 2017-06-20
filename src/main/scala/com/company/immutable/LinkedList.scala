package com.company.immutable

import scala.reflect.ClassTag

/**
  * Created by mrv on 11.04.17.
  */
class LinkedList[T <: Ordered[T]] extends Seq[T] {

  private var front: LinkedList[T]#Entry = _
  private var back: LinkedList[T]#Entry = _
  private var listSize: Int = 0
  private var visitor: LinkedListSortVisitor[T] = _

  def accept(visitor: LinkedListSortVisitor[T]): Unit = {
    this.visitor = visitor
  }

  def mergeSort(): LinkedList[T] = {

    def sort(list: LinkedList[T]): LinkedList[T] = {

      if (list.listSize == 1) {
        return list
      }

      val (firstHalf, secondHalf) = list.splitAt(list.listSize / 2)

      sort(firstHalf) merge sort(secondHalf)

    }

    sort(this)

  }

  def remove(index: Int): LinkedList[T] = {
    checkIndex(index)

    val list: LinkedList[T] = copy()

    list.listSize -= 1
    if (index == 0) {
      list.front = list.front.next
      return list
    }
    val it = list.getEntry(index - 1)
    it.next = it.next.next
    list
  }

  def insert(index: Int, value: T): LinkedList[T] = {

    val list: LinkedList[T] = copy()
    list.listSize += 1
    if (index == listSize) {
      list.append(value)
      return list
    }

    checkIndex(index)

    if (index == 0) {
      val tempFront = list.front
      list.front = new Entry(value)
      list.front.next = tempFront
      return list
    }

    val it = list.getEntry(index - 1)

    val itNext = it.next
    it.next = new Entry(value)
    it.next.next = itNext

    list
  }

  def set(index: Int, value: T): LinkedList[T] = {
    checkIndex(index)
    val list: LinkedList[T] = copy()
    val it = list.getEntry(index)
    it.value = value
    list
  }

  override def size(): Int = listSize

  override def toString: String = {
    val stringBuilder: StringBuilder = new StringBuilder

    if (isEmpty) {
      return stringBuilder
        .append("[]")
        .toString()
    }

    stringBuilder.append("[")
    foreach(elem => stringBuilder.append(elem.toString).append(", "))
    stringBuilder.delete(stringBuilder.length - 2, stringBuilder.length)
    stringBuilder.append("]")
    stringBuilder.toString
  }

  override final def foreach[U](f: T => U): Unit = {

    var it = front
    while (it != null) {
      f(it.value)
      it = it.next
    }

  }

  def apply(index: Int): T = {
    getEntry(index).value
  }

  private def getEntry(index: Int): LinkedList[T]#Entry = {
    checkIndex(index)
    var it = front
    for (_ <- 1 to index) {
      it = it.next
    }
    it
  }

  private def checkIndex(index: Int): Unit = {

    if (isEmpty) {
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds!")
    }

    if (index < 0 || index >= listSize) {
      throw new IndexOutOfBoundsException(s"Index $index is out of bounds!")
    }

  }

  override def isEmpty: Boolean = listSize == 0

  def add(value: T): LinkedList[T] = {
    val list: LinkedList[T] = copy()
    list.append(value)
    list
  }

  def ++(that: LinkedList[T]): LinkedList[T] = {
    val thisCopy: LinkedList[T] = copy()
    that.foreach(value => thisCopy.append(value))
    thisCopy
  }

  override def toArray[U >: T](implicit m: ClassTag[U]): Array[U] = {
    val array = new Array[U](listSize)
    var it = front
    for (i <- 0 until listSize) {
      array(i) = it.value
      it = it.next
    }
    array
  }

  override def length: Int = listSize

  override def iterator: Iterator[T] = {

    val it = new Iterator[T]() {

      private var entry : LinkedList[T]#Entry = front

      override def hasNext: Boolean = {
        entry != null
      }

      override def next(): T = {
        val temp = entry.value
        entry = entry.next
        temp
      }
    }
    it
  }

  override def splitAt(index: Int): (LinkedList[T], LinkedList[T]) = {
    checkIndex(index)

    if (visitor != null) {
      visitor.visitSplitBegin(this, index)
    }

    val firstPart = new LinkedList[T]
    val secondPart = new LinkedList[T]

    firstPart.front = this.front
    firstPart.back = this.getEntry(index - 1)
    firstPart.listSize = index

    secondPart.front = this.getEntry(index)
    secondPart.back = this.back
    secondPart.listSize = this.listSize - index

    firstPart.back.next = null

    if (visitor != null) {
      visitor.visitSplitEnd(firstPart, secondPart)
    }

    firstPart.accept(visitor)
    secondPart.accept(visitor)

    (firstPart, secondPart)
  }

  private def merge(that: LinkedList[T]): LinkedList[T] = {

    if (that == null) {
      throw new NullPointerException()
    }

    if (visitor != null) {
      visitor.visitMergeBegin(this, that)
    }


    var thisIt = front
    var thatIt = that.front

    val result: LinkedList[T] = new LinkedList[T]
    while (thisIt != null || thatIt != null) {

      if (thisIt == null) {
        result.append(thatIt)
        thatIt = thatIt.next
      } else if (thatIt == null) {
        result.append(thisIt)
        thisIt = thisIt.next
      } else if (thisIt.value < thatIt.value) {
        result.append(thisIt)
        thisIt = thisIt.next
      } else {
        result.append(thatIt)
        thatIt = thatIt.next
      }
    }

    result.accept(visitor)

    if (visitor != null) {
      visitor.visitMergeEnd(result)
    }

    result
  }

  private def append(entry: LinkedList[T]#Entry): Unit = {
    if (isEmpty) {
      front = entry
      back = front
    } else {
      back.next = entry
      back = back.next
    }
    listSize += 1
  }

  private def copy(): LinkedList[T] = {
    val thisCopy: LinkedList[T] = new LinkedList[T]
    foreach(value => thisCopy.append(value))
    thisCopy
  }

  private def append(value: T): Unit = {
    if (isEmpty) {
      front = new Entry(value)
      back = front
    } else {
      back.next = new Entry(value)
      back = back.next
    }
    listSize += 1
  }

  class Entry(var value: T) {
    var next: LinkedList[T]#Entry = _
    override def toString: String = value.toString
  }
}
