import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


class MutableTreeMapTest extends FunSuite {
  val prefix = "mutable.TreeMap"
  
  test("creation of empty Map") {
    val emptyMap = TreeMap[Int, Int]()
    assert(emptyMap.isEmpty)
    assert(emptyMap.toString === prefix + "()")
    
    val elem = ((1, 2))
    val mapWithAElem = TreeMap(elem)
    assert(!mapWithAElem.isEmpty)
    assert(mapWithAElem.toString === prefix + "("+toAssocArrowString(elem)+")")
    
    val sortedElems = Seq((1, 2), (3, 4), (5, 6))
    val mapFromSortedElems = TreeMap(sortedElems: _*)
    assert(!mapFromSortedElems.isEmpty)
    assert(mapFromSortedElems.toString === prefix + toAssocArrowString(sortedElems))
    
    val unsortedElems = Seq((10, 2), (3, 4), (5, 6))
    val mapFromUnsortedElems = TreeMap(unsortedElems: _*)
    assert(!mapFromUnsortedElems.isEmpty)
    assert(mapFromUnsortedElems.toString === prefix + toAssocArrowString(unsortedElems.sorted))
  }
  
  test ("append") {
    val map = TreeMap[Int, Int]()
    val seq = Seq((1, 2), (10, 8), (56, 23), (40, 645), (432, 54))
    
    for (((k, v), idx) <- seq zipWithIndex) {
      map += ((k, v))
      assert(map.toSeq === seq.take(idx + 1).sorted)
    }
    
    // update an existing element
    val oldhead = seq.head
    val newhead = (oldhead._1, oldhead._2 + 10)
    map += (newhead)
    assert(map.toSeq === (seq.tail :+ newhead).sorted)
  }
  
  
  // helper methods
  def toAssocArrowString[A, B](t: (A, B)): String = 
    t._1.toString+" -> "+t._2.toString
    
  def toAssocArrowString[A, B](s: Seq[(A, B)]): String =
    s map {toAssocArrowString(_)} mkString("(", ", ", ")")
    		
}
