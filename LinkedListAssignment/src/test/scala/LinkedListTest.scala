import org.scalatest.funsuite.AnyFunSuite

class LinkedListTest extends AnyFunSuite{

  val linkedlistInt = new Linkedlist[Int]
  val linkedlistLong = new Linkedlist[Long]

  test("Test case 1  for insert element ") {
    assert(linkedlistInt.head==null)
    linkedlistInt.insertElement(23)
    assert(linkedlistInt.head.data===23)
  }

  test("Test case 2 for Delete element "){
    linkedlistInt.insertElement(2)
    linkedlistInt.insertElement(3)
    linkedlistInt.insertElement(4)
    /*
     4 ->3->2
     after delete  4->3
     */
    linkedlistInt.delete(2)
    assert(linkedlistInt.head.data===4)
    assert(linkedlistInt.head.next.data===3)
  }


  test("Test case 3  for  Search Element in LinkedList") {
    linkedlistInt.insertElement(1)
    linkedlistInt.insertElement(3)
    linkedlistInt.insertElement(4)
    val searchElement = linkedlistInt.search(2)
    val searchForAnotherElement =linkedlistInt.search(4).get.data
    assert(searchForAnotherElement===4)
    assert(searchElement.isEmpty)
  }


  test("Test case 4  for insert element of Long Type ") {
    linkedlistLong.insertElement(23L)
    assert(linkedlistLong.head.data === 23L)
  }

  test("Test case 5 for Delete element of Long type") {
    linkedlistLong.insertElement(2543543L)
    linkedlistLong.insertElement(37657657L)
    linkedlistLong.insertElement(47667576L)
    /*
     47667576->37657657->2543543
     after delete 47667576->37657657
     */
    linkedlistLong.delete(2543543L)
    assert(linkedlistLong.head.data === 47667576L)
    assert(linkedlistLong.head.next.data === 37657657L)
  }


  test("Test case 6  for  Search Element in LinkedList  of Long type") {
    linkedlistLong.insertElement(1126432)
    linkedlistLong.insertElement(365765765)
    linkedlistLong.insertElement(47576576)
    val searchElement = linkedlistLong.search(6)
    val searchForAnotherElement = linkedlistLong.search(47576576).get.data
    assert(searchForAnotherElement === 47576576)
    assert(searchElement.isEmpty)
  }


}
