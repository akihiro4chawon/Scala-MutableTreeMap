import scala.collection.generic.SortedMapFactory
import scala.collection.{SortedMap, SortedMapLike}
import scala.collection.mutable.{Builder, MapLike, Map}

object TreeMap extends SortedMapFactory[TreeMap] {
  // implements empty
  override def empty[A, B](implicit ord: Ordering[A]) = new TreeMap[A, B]
  
  // overrides newBuilder for efficiency (mutable.MapLike is Builder of itself)
  override def newBuilder[A, B](implicit ord: Ordering[A]): Builder[(A, B), TreeMap[A, B]] = new TreeMap[A, B] 
  
  // avoid ambiguity issues
  implicit def canBuildFrom[A, B](implicit ord: Ordering[A]) = new SortedMapCanBuildFrom[A, B]
}

class TreeMap[A, B] private (private var root: LLRBTree[A, B]#Node)
  (implicit val ordering: Ordering[A])
  extends LLRBTree[A, B]()(ordering)
  with SortedMap[A, B]
  with SortedMapLike[A, B, TreeMap[A, B]]
  with Map[A, B]
  with MapLike[A, B, TreeMap[A, B]]
{
  type Node = LLRBTree[A, B]#Node
  
  def this()(implicit ord: Ordering[A]) =
    this(null)(ord)
  if (root == null)
    root = Empty

  // overrides collection.mutable.Map.+=
  override def +=(entry: (A, B)) = {
    root = root.insert(entry._1, entry._2)
    if (!root.isEmpty) root.isRed = false
    this
  }

  // overrides collection.mutable.Map.-=
  override def -=(k: A) = {
    root = root.delete(k)
    if (!root.isEmpty) root.isRed = false
    this
  }
  
  // overrides collection.MapLike.get
  override def get(k: A): Option[B] = root.get(k) match {
    case Empty => None
    case nonEmpty : ColoredNode => Some(nonEmpty.value)
  }
  
  // overrides collection.MapLike.iterator
  override def iterator: Iterator[(A, B)] = {
    
    // iterator that traverses a binary tree in in-order
    class InOrderIterator extends Iterator[(A, B)] {
      private type StackT = collection.mutable.Stack[(Node, Boolean)]
      private trait RejectingEmptyNode extends StackT {
        // reject `Empty` node
        abstract override def push(elem: (Node, Boolean)) =
          if (elem._1.isEmpty) this else super.push(elem)
        // a non-empty node is guaranteed
        abstract override def pop: (ColoredNode, Boolean) =
          super.pop().asInstanceOf[(ColoredNode, Boolean)]
      }
      private val stack = new StackT with RejectingEmptyNode   
      stack.push((root, false))
      
      @scala.annotation.tailrec
      private def getNext: Option[ColoredNode] = {
        if (stack.isEmpty) None
        else {
          val (node, isLeftVisited) = stack.pop()
          if (!isLeftVisited) {
            stack.push((node, true), (node.left, false))
            getNext
          } else {
            stack.push((node.right, false))
            Some(node)
          }
        }
      }
      
      override def hasNext = !(stack.isEmpty)
      override def next = getNext match {
        case None => 
          throw new NoSuchElementException("next on empty iterator")
        case Some(node) =>
          (node.key, node.value)
      }
    }
    
    new InOrderIterator
  }
  
  // overrides foreach for efficiency
  override def foreach[U](f: ((A, B)) => U) {
    def recurseInOrder(node: Node) {
      node match {
        case Empty => /* do nothing */
        case node: ColoredNode => {
          recurseInOrder(node.left)
          f((node.key, node.value))
          recurseInOrder(node.right)
        }
      }
    }
    recurseInOrder(root)
  }
  
  // overrides collection.SortedMap.rangeImpl 
  override def rangeImpl(from: Option[A], until: Option[A]) =
    new TreeMap[A, B](root.range(from, until))
  
  // overrides size for efficiency (otherwise, default implementation of TraversableLike is used)
  // but, current implementation is far from efficient one
  override def size: Int = root.count
  
  // yes, really mutable!
  override def stringPrefix = "mutable.TreeMap"

  // avoid type conflicts between collection.SortedMapLike and mutable.MapLike
  override def empty = new TreeMap[A, B]()(ordering)
  override def updated[B1 >: B](key: A, value: B1): TreeMap[A, B1] = this + ((key, value))
  override def + [B1 >: B] (kv: (A, B1)): TreeMap[A, B1] = clone.asInstanceOf[TreeMap[A, B1]] += kv
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): TreeMap[A, B1] =
    clone.asInstanceOf[TreeMap[A, B1]] += elem1 += elem2 ++= elems

}




