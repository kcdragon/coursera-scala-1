package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class GameDefSuite extends FunSuite {
  trait GameDefImpl extends StringParserTerrain {
    val level =
    """oT
      |oo
      |So""".stripMargin
  }

  test("isStanding: true if block is in a standing position") {
    new GameDefImpl() {
      val block = new Block(new Pos(1,2), new Pos(1,2))
      assert(block.isStanding)
    }
  }

  test("isStanding: false if block is not in a standing position") {
    new GameDefImpl() {
      val block = new Block(new Pos(1,2), new Pos(2,2))
      assert(!block.isStanding)
    }
  }

  test("isLegal: true if both positions are on terrain") {
    new GameDefImpl() {
      val block = new Block(new Pos(1,0), new Pos(2,0))
      assert(block.isLegal)
    }
  }

  test("isLegal: false if one position is off terrain") {
    new GameDefImpl() {
      val block = new Block(new Pos(2,0), new Pos(3,0))
      assert(!block.isLegal)
    }
  }

  test("isLegal: false if both positions are off terrain") {
    new GameDefImpl() {
      val block = new Block(new Pos(3,0), new Pos(4,0))
      assert(!block.isLegal)
    }
  }

  test("startBlock: block located at S position") {
    new GameDefImpl() {
      assert(startBlock == new Block(new Pos(2,0), new Pos(2,0)))
    }
  }

  test("neighbors") {
    new GameDefImpl() {
      val block = new Block(new Pos(2,0), new Pos(2,0))
      assert(block.neighbors contains (block.left, Left))
      assert(block.neighbors contains (block.right, Right))
      assert(block.neighbors contains (block.up, Up))
      assert(block.neighbors contains (block.down, Down))
    }
  }

  test("legalNeighbors") {
    new GameDefImpl() {
      val block = new Block(new Pos(2,0), new Pos(2,0))
      assert(block.legalNeighbors contains (block.up, Up))
    }
  }
}
