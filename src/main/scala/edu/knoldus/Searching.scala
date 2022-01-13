package edu.knoldus

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    def search(array: Array[Int], elem: Int, start: Int, end: Int): Boolean = {
      if (start > end) return false

      val mid = start + (end - start) / 2
      if (array(mid) == elem) true
      else if (array(mid) > elem) {
        search(array, elem, start, mid - 1)
      }
      else {
        search(array, elem, mid + 1, end)
      }
    }
    search(array, elem, 0, array.length - 1)
  }


  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    if (array.isEmpty) {
      false
    } else if (array.head == elem) {
      true
    } else {
      linearSearch(array.tail, elem)
    }
  }

}
