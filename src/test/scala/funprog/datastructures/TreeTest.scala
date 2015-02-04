package funprog.datastructures

import org.scalacheck.Gen
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object TreeTest extends Specification with ScalaCheck {
  import Tree._

  "i can also build trees" in {
    "that have a size" in {
      Tree.size(
        Branch(
          Branch(
            Leaf("a"), Leaf("b")),
          Branch(
            Leaf("c"), Leaf("d")))) mustEqual 7
      sizeFold(
        Branch(
          Branch(
            Leaf(1), Leaf(2)),
          Branch(
            Leaf(3), Branch(Leaf(4), Leaf(5))))) mustEqual 9
    }
    "that have a maximum" in {
      maximum(
        Branch(
          Branch(
            Leaf(1), Leaf(2)),
          Branch(
            Leaf(3), Leaf(4)))) mustEqual 4
      maximumFold(
        Branch(
          Branch(
            Leaf(1), Leaf(2)),
          Branch(
            Leaf(3), Leaf(4)))) mustEqual 4
    }

    "that have a depth" in {
      depth(
        Branch(
          Branch(
            Leaf(1), Leaf(2)),
          Branch(
            Leaf(3), Branch(Leaf(4), Leaf(5))))) mustEqual 4
      depthFold(
        Branch(
          Branch(
            Leaf(1), Leaf(2)),
          Branch(
            Leaf(3), Branch(Leaf(4), Leaf(5))))) mustEqual 4
    }
    "that i can map functions over" in {
      val tree = Branch(
        Branch(
          Leaf(1), Leaf(2)),
        Branch(
          Leaf(3), Branch(Leaf(4), Leaf(5))))

      val mapRes = Tree.map(tree)(_ * 2)
      val foldRes = mapFold(tree)(_ * 2)

      mapRes mustEqual foldRes
      foldRes mustEqual Branch(
          Branch(
            Leaf(2), Leaf(4)),
          Branch(
            Leaf(6), Branch(Leaf(8), Leaf(10))))
    }
  }
}
