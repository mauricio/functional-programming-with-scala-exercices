package chapter3

import org.specs2.mutable.Specification

/**
 * User: mauricio
 * Date: 9/7/13
 * Time: 5:23 PM
 */
class TreeSpecs extends Specification {

  "trees" should {

    "correctly calculate the size" in {
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
      Tree.size(tree) === 7
    }

    "correctly calculate maximum number" in {
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(4), Leaf(3)))
      Tree.maximum(tree) === 4
    }

    "correctly calculate deepest leaf" in {
      val tree = Branch(
        Branch(
          Leaf(1),
          Leaf(2)),
        Branch(
          Leaf(4),
          Branch(
            Branch(
              Leaf(1),
              Leaf(2) ),
            Leaf(3)
          )
        )
      )
      Tree.depth(tree) === 5
    }

    "correctly map the tree" in {
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
      val mapped = Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))
      Tree.map(tree) { x => x * 2 } === mapped
    }

  }

}
