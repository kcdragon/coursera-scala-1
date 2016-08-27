package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class SolverSuite extends FunSuite {
  trait GameDefImpl extends Solver with StringParserTerrain {
    val level =
    """oT
      |oo
      |So""".stripMargin
  }

  test("done: true if block is in goal position") {
    new GameDefImpl() {
      val block = new Block(new Pos(0, 1), new Pos(0, 1))
      assert(done(block))
    }
  }

  test("done: false if block is not in goal position") {
    new GameDefImpl() {
      val block = new Block(new Pos(0, 1), new Pos(1, 1))
      assert(!done(block))
    }
  }
}
