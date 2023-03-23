import scala.annotation.tailrec

//created Node
class Node[A](var data: A, var nextElement: Node[A])

//Generic  LinkedList class
class GenericLinkedlist[A] {

//created Null head
  var head: Node[A] = null

  //created newNode
  def insertElement(data: A): Unit = {
    val newNode = new Node[A](data, null)
    if (head == null) {
      head = newNode
    } else {
      newNode.nextElement = head
      head = newNode
    }
  }

  // Delete a Node in a LinkedList
  def delete(data: A): Unit = {
    @tailrec
    def deleteElement(previousElement: Node[A], currentElement: Node[A]): Unit = {
      if (currentElement == null) return
      if (currentElement.data == data) {
        previousElement.nextElement = currentElement.nextElement
      } else {
        deleteElement(currentElement, currentElement.nextElement)
      }
    }
    deleteElement(head, head.nextElement)
  }

// Traverse th LinkedList
  def traverse(): Unit = {
    @tailrec
    def traverseLinkedList(currentElement: Node[A]): Unit = {
      if (currentElement != null) {
        println(currentElement.data)
        traverseLinkedList(currentElement.nextElement)
      }
    }
    traverseLinkedList(head)
  }

//Search Element in The linkedList
  def search(data: A): Option[Node[A]] = {
    @tailrec
    def searchElement(current: Node[A]): Option[Node[A]] = {
      if (current == null) {
        None
      } else if (current.data == data) {
        Some(current)
      } else {
        searchElement(current.nextElement)
      }
    }
    searchElement(head)
  }
}


