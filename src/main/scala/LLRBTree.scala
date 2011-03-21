

class LLRBTree[Key, Value](implicit val ord: Ordering[Key]) {
  def compareKeys(x: Key, y: Key) = ord.compare(x, y)
  
  // Interface of Node
  sealed abstract class Node {
    def isEmpty: Boolean
    var isRed: Boolean
    var left: Node
    var right: Node
    def insert(k: Key, v: Value): Node
    def delete(k: Key): Node
    def get(k: Key): Node
    def range(from: Option[Key], until: Option[Key]): Node
    def first: Node
    def last: Node
    def count: Int
  }
  
  // Concrete Non-Empty Node
  case class ColoredNode(
    var key: Key,
    var value: Value,
    override var left: Node,
    override var right: Node,
    override var isRed: Boolean
  ) extends Node {
    
    override def isEmpty = false
    
    override def insert(k: Key, v: Value): Node = {
      if (left.isRed && right.isRed) flipColor  // split 4-node
      compareKeys(k, key) match {
        case cmp if (cmp < 0) => left  = left.insert(k, v)
        case cmp if (cmp > 0) => right = right.insert(k, v)
        case 0 => value = v
      }
      val h = if (right.isRed) rotateLeft else this
      if (h.left.isRed && h.left.left.isRed) h.rotateRight else h
    }
    
    private def moveRedLeft: ColoredNode = {
      flipColor
      if (right.left.isRed) {
        right = right.asInstanceOf[ColoredNode].rotateRight
        val h = rotateLeft; h.flipColor; h
      } else this
    }
    
    private def moveRedRight: ColoredNode = {
      flipColor
      if (left.left.isRed) {
        val h = rotateRight; h.flipColor; h
      } else this
    }
    
    private def fixUp: ColoredNode = {
      var node = this
      if (node.right.isRed)
        node = node.rotateLeft
      if (node.left.isRed && node.left.left.isRed)
        node = node.rotateRight
      if (node.left.isRed && node.right.isRed)
        node.flipColor
      node
    }
    
    override def delete(k: Key): Node = {
      var node = this
      if (compareKeys(k, key) < 0) {
        if (!left.isRed && !left.left.isRed)
          node = node.moveRedLeft
        node.left = node.left.delete(k)
      } else {
        if (node.left.isRed)
          node = node.rotateRight
        if (compareKeys(k, node.key) == 0 && node.right.isEmpty)
          return Empty
        if (!node.right.isRed && !node.right.left.isRed)
          node = node.moveRedRight
        if (compareKeys(k, node.key) == 0) {
          val (newNode, minNode) = node.right.asInstanceOf[ColoredNode].popMinimum
          node.key = minNode.asInstanceOf[ColoredNode].key
          node.value = minNode.asInstanceOf[ColoredNode].value
          node.right = newNode
        } else node.right = node.right.delete(k)
      }
      node.fixUp
    }
    
    override def get(k: Key): Node = {
      compareKeys(k, key) match {
        case cmp if cmp < 0 => left.get(k)
        case cmp if cmp > 0 => right.get(k)
        case 0 => this
      }
    }
    
    def popMinimum: (Node, Node) = {
      var minNode: Node = Empty
      def rec(n: ColoredNode): Node = {
        if (n.left.isEmpty) {
          minNode = n
          Empty
        } else {
          var node = n
          if (!node.left.isRed && !node.left.left.isRed)
            node = node.moveRedLeft
          node.left = rec(node.left.asInstanceOf[ColoredNode])
          node.fixUp
        }
      }
      (rec(this), minNode)
    }
    
    final private def flipColor {
      isRed = !isRed
      left.isRed = !left.isRed
      right.isRed = !right.isRed
    }
    
    final private def rotateRight: ColoredNode = {
      val l = left.asInstanceOf[ColoredNode]
      left = l.right 
      l.right = this
      l.isRed = isRed
      isRed = true
      l
    }
    
    final private def rotateLeft: ColoredNode = {
      val r = right.asInstanceOf[ColoredNode]
      right = r.left
      r.left = this
      r.isRed = isRed
      isRed = true
      r
    }
    
    override def range(from: Option[Key], until: Option[Key]): Node = (from, until) match {
      case (None, None) => this
      case (Some(f), _) if compareKeys(key, f) < 0 => right.range(from, until)
      case (_, Some(u)) if compareKeys(u, key) <= 0 => left.range(from, until)
      case _ => {
        val newLeft = left.range(from, None)
        val newRight = right.range(None, until)
        if ((newLeft eq left) && (newRight eq right)) this
        else if (newLeft eq Empty) newRight.insert(key, value)
        else if (newRight eq Empty) newLeft.insert(key, value)
        else ColoredNode(key, value, newLeft, newRight, isRed)
      }
    }
    
    override def first = if (left.isEmpty) this else left.first
    override def last = if (right.isEmpty) this else right.last
    override def count = 1 + left.count + right.count
  }
 
  // Empty-Node Singleton
  case object Empty extends Node {
    def isEmpty = true
    override def isRed = false
    override def isRed_= (x: Boolean) {assert(!x)}
    override def left = this
    override def left_= (x: Node) {throw new Error("Misimplementation of LLRBTree") }
    override def right = this
    override def right_=(x: Node) {throw new Error("Misimplementation of LLRBTree") }
    override def insert(k: Key, v: Value): Node = ColoredNode(k, v, Empty, Empty, true)
    override def delete(k: Key): Node = this
    override def get(k: Key) = this
    override def range(from: Option[Key], until: Option[Key]): Node = this
    override def first = this
    override def last = this
    override def count = 0
  }
}
