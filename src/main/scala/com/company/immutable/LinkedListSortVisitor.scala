package com.company.immutable


/**
  * Created by mrv on 12.04.17.
  */
trait LinkedListSortVisitor[T <: Ordered[T]] {

  def visitSplitBegin(originalList: LinkedList[T], splitPos: Int)

  def visitSplitEnd(firstList: LinkedList[T], secondList: LinkedList[T])

  def visitMergeBegin(firstList: LinkedList[T], secondList: LinkedList[T])

  def visitMergeEnd(resultList: LinkedList[T])

}
