import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
 
class LLRBTreeSuite extends LLRBTree[Int, Int] with FunSuite {
  
  test("insert is invoked on a Empty node") {
    val magicKey = 12345
    val magicVal = 67890
    
    var root: Node = Empty
    root = root.insert(magicKey, magicVal)
    assert(root.count === 1)
    assert(!root.isEmpty)
    root match {
      case ColoredNode(k, v, l, r, isRed) => {
        assert(k === magicKey)
        assert(v === magicVal)
        assert(l === Empty)
        assert(r === Empty)
        assert(isRed) // to be modified
      }
      case _ => fail("root is not a colored node")
    }
    assert(root.toString === "ColoredNode("+magicKey+","+magicVal+",Empty,Empty,true)") // isRed == true??
    assert(root.get(magicKey) eq root)
    assert(root.first eq root)
    assert(root.last eq root)
    validateLLRBTree(root)
  }
  
  
  test ("delete is invoked on a Empty node") {
    var root: Node = Empty
    root = root.delete(1)
    assert(root === Empty)
  }
  
  
  test ("insert an increasing sequence number") {
    val keys = 1 to 10
    val minKey = keys.head
    val vals = keys map {_ * 2}
    val minVal = vals.head
    
    var root: Node = Empty
    for ((k, v) <- keys zip vals) {
      println("-- insert " + k + " --")
      root = root.insert(k, v)
      root.isRed = false
      printNode(root, 0)
      
      validateLLRBTree(root)
      assert(root.count === k)
      assert(root.first match {
        case ColoredNode(`minKey`, `minVal`, _, _, _) => true
        case _ => false
      })
      assert(root.last match {
        case ColoredNode(`k`, `v`, _, _, _) => true
        case _ => false
      })
      assert(!root.isEmpty)
    }
    
    for ((k, v) <- keys zip vals) {
      root.get(k) match {
        case ColoredNode(k2, v2, _, _, _) => {
          assert(k === k2)
          assert(v === v2)
        }
        case _ => fail("node not found #" + (k, v))
      }
    }
    assert(100 to 200 forall {root.get(_) == Empty})
    
    for ((k, v) <- keys zip vals) {
      println("-- delete " + k + " --")
      root = root.delete(k)
      root.isRed = false
      printNode(root, 0)
      validateLLRBTree(root)
    }    
  }
  
  
  test ("range works properly") {
    val keys = 1 to 100
    val vals = keys map {_ * 3}
    
    var root: Node = Empty
    for ((k, v) <- keys zip vals) {
      root = root.insert(k, v)
      root.isRed = false
    }
    
    val identicalSubTree = root.range(None, None)
    validateLLRBTree(identicalSubTree)
    assert(root eq identicalSubTree)
    
    val subTree1 = root.range(Some(90), None)
    // current range implementation does not guarantee balance of the returned subtree
//    validateLLRBTree(subTree1)
    assert(1 to 89 forall {subTree1.get(_) == Empty})
    assert(90 to 100 forall {subTree1.get(_).isInstanceOf[ColoredNode]})
    
    val subTree2 = root.range(None, Some(10))
    // current range implementation does not guarantee balance of the returned subtree
//    validateLLRBTree(subTree2)
    assert(1 to 9 forall {subTree2.get(_).isInstanceOf[ColoredNode]})
    assert(10 to 100 forall {subTree2.get(_) == Empty})
    
    val subTree3 = root.range(Some(40), Some(60))
    // current range implementation does not guarantee balance of the returned subtree
//    validateLLRBTree(subTree3)
    assert(1 to 39 forall {subTree3.get(_) == Empty})
    assert(40 to 59 forall {subTree3.get(_).isInstanceOf[ColoredNode]})
    assert(60 to 100 forall {subTree3.get(_) == Empty})
  }

  
  
  // helper method that validates LLRBTree
  def validateLLRBTree(node: Node): Int = {
    if (node.isEmpty) 0
    else {
      !node.left.isRed && node.right.isRed
      assert(node.left.isRed || !node.right.isRed, "A red child should be left")
      assert(!(node.isRed && node.left.isRed), "Consecutive left leaning red children")
      val ldepth = validateLLRBTree(node.left)
      val rdepth = validateLLRBTree(node.right)
      assert(ldepth === rdepth, "Unbalanced tree")
      ldepth + (if (node.isRed) 0 else 1)
    }
  }
  
  // helper method that print LLRBTree
  def printNode(node: Node, depth: Int): Unit = node match {
    case Empty => ()
    case ColoredNode(key, value, left, right, isRed) => {
      printNode(left, depth + 1)
      println(("    " * depth) + (if (isRed) "R" else "B") + key)
      printNode(right, depth + 1)
    }
  }  
}
