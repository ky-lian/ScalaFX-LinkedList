package hello

import com.company.immutable.{LinkedList, LinkedListSortVisitor}

import scalafx.scene.control.TextArea

/**
  * Created by mrv on 09.05.17.
  */
class LinkedListSortLogVisitor[T <: Ordered[T]](val logArea: TextArea) extends LinkedListSortVisitor[T] {

  var opCounter: Int = 0

  override def visitSplitEnd(firstList: LinkedList[T], secondList: LinkedList[T]): Unit = {
    logArea.setText(logArea.text.value + s"\n$firstList and $secondList;")
  }

  override def visitSplitBegin(originalList: LinkedList[T], splitPos: Int): Unit = {
    opCounter += 1
    logArea.setText(logArea.text.value + s"\n$opCounter. List $originalList was split at $splitPos into:")
  }

  override def visitMergeBegin(firstList: LinkedList[T], secondList: LinkedList[T]): Unit = {
    opCounter += 1
    logArea.setText(logArea.text.value + s"\n$opCounter. Merged $firstList and $secondList into:")
  }

  override def visitMergeEnd(resultList: LinkedList[T]): Unit = {
    logArea.setText(logArea.text.value + s"\n$resultList;")
  }
}
