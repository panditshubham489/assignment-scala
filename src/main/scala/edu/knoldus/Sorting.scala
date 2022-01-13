package edu.knoldus
import scala.annotation.tailrec
class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    val myList: List[Int] = array.toList
    def helperInsertionSort(list: List[Int]): List[Int] =
      if (list.isEmpty) Nil
      else insert(list.head, helperInsertionSort(list.tail))

    def insert(x: Int, list: List[Int]): List[Int] =
      if (list.isEmpty || x <= list.head) x :: list
      else list.head :: insert(x, list.tail)

    helperInsertionSort(myList).toArray
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def innerSelection(array: Array[Int], list: List[Int] = List()): Array[Int] = {
      val min = array.min
      val anOtherList = list ++ array.toList.filter(_ == min)
      if (array.min == array.max) anOtherList.toArray
      else innerSelection(array.filter(_ > min), anOtherList)
    }
    innerSelection(array)
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def innerBubble(array: Array[Int], len: Int): Int = {
      if (len == 1) return 0
      for (i <- 0 until len - 1) {
        if (array(i) > array(i + 1)) {
          val temp = array(i)
          array(i) = array(i + 1)
          array(i + 1) = temp
        }
      }
      innerBubble(array, len - 1)
    }

    innerBubble(array, array.length)
    array
  }

}