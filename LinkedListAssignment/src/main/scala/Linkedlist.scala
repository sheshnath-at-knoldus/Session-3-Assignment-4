import scala.annotation.tailrec

//created Node
class Node[A](var data: A, var next: Node[A])

//Generic  LinkedList class
class Linkedlist[A] {

//created Null head
  var head: Node[A] = null

  //created newNode
  def insertElement(data: A): Unit = {
    val newNode = new Node[A](data, null)
    if (head == null) {
      head = newNode
    } else {
      newNode.next = head
      head = newNode
    }
  }

  // Delete a Node in a LinkedList
  def delete(data: A): Unit = {
    @tailrec
    def deleteElement(prev: Node[A], current: Node[A]): Unit = {
      if (current == null) return
      if (current.data == data) {
        prev.next = current.next
      } else {
        deleteElement(current, current.next)
      }
    }
    deleteElement(head, head.next)
  }


  def traverse(): Unit = {
    @tailrec
    def traverseLinkedList(current: Node[A]): Unit = {
      if (current != null) {
        println(current.data)
        traverseLinkedList(current.next)
      }
    }
    traverseLinkedList(head)
  }


  def search(data: A): Option[Node[A]] = {
    @tailrec
    def searchElement(current: Node[A]): Option[Node[A]] = {
      if (current == null) {
        None
      } else if (current.data == data) {
        Some(current)
      } else {
        searchElement(current.next)
      }
    }
    searchElement(head)
  }
}


