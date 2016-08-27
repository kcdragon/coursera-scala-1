package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {
  test("terrain: empty Vector") {
    trait EmptyLevel extends StringParserTerrain {
      val level = "-"
    }
    new EmptyLevel() {
      assert(!terrain(Pos(0, 0)))
    }
  }

  test("terrain: one row, two columns") {
    trait OneRowTwoColumnsLevel extends StringParserTerrain {
      val level = "o-"
    }
    new OneRowTwoColumnsLevel() {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(0, 1)), "0,1")

      // out of bounds
      assert(!terrain(Pos(1, 0)), "1,0")
      assert(!terrain(Pos(-1, 0)), "-1,0")
    }
  }

  test("terrain: two rows, one column") {
    trait TwoRowsOneColumnLevel extends StringParserTerrain {
      val level =
      """-
        |o""".stripMargin
    }
    new TwoRowsOneColumnLevel() {
      assert(!terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 0)), "1,0")

      // out of bounds
      assert(!terrain(Pos(0, 1)), "0,1")
      assert(!terrain(Pos(0, -1)), "0,-11")
    }
  }

  test("findChar: char in row") {
    trait Level extends StringParserTerrain {
      val level = "SoT"
    }
    new Level() {
      assert(startPos == Pos(0,0))
      assert(goal == Pos(0,2))
    }
  }

  test("findChar: char in column") {
    trait Level extends StringParserTerrain {
      val level =
      """S
        |o
        |T""".stripMargin
    }
    new Level() {
      assert(startPos == Pos(0,0))
      assert(goal == Pos(2,0))
    }
  }
}
