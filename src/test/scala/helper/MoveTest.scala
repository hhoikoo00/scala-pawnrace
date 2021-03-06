package helper

import org.scalatest.funsuite.AnyFunSuite

class MoveTest extends AnyFunSuite {

  test("straight-forward move has correct SAN representation") {
    val moveTest = new Move(new Square(1, 1), new Square(3, 1), false, false)
    assertResult("b4")(moveTest.asSAN)

    val moveTest2 = new Move(new Square(6, 3), new Square(5, 3), false, false)
    assertResult("d6")(moveTest2.asSAN)
  }

  test("captures and en passant captures have correct SAN representation") {
    val moveTest = new Move(new Square(2, 6), new Square(3, 7), true, false)
    assertResult("gxh4")(moveTest.asSAN)

    val moveTest2 = new Move(new Square(4, 1), new Square(5, 2), false, true)
    assertResult("bxc6")(moveTest2.asSAN)
  }

  test("equals works as intended") {
    val square1 = new Square(1, 1)
    val square2 = new Square(2, 2)
    val square2_ = new Square(2, 2)

    assert(new Move(square1, square2, false, false) ==
      new Move(square1, square2, false, false))
    assertResult(false)(new Move(square1, square2, false, false) ==
      new Move(square1, square2_, false, false))
  }
}
