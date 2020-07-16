package helper

import org.scalatest.funsuite.AnyFunSuite

class SquareTest extends AnyFunSuite {

  test("constructor correctly initializes the parameters") {
    val squareTest = new Square(x = 1, y = 2, occupier = Color.BLACK);

    assertResult(1)(squareTest.x)
    assertResult(2)(squareTest.y)
    assertResult(Color.BLACK)(squareTest.occupier)

    assertResult('2')(squareTest.rank)
    assertResult('c')(squareTest.file)
  }

  test("default occupier is None") {
    val squareTest = new Square(1, 2)

    assertResult(Color.NONE)(squareTest.occupier)
  }

  test("invalid parameters throw IllegalArgumentException") {
    assertThrows[IllegalArgumentException](new Square(-1, 2))
    assertThrows[IllegalArgumentException](new Square(1, -2))
    assertThrows[IllegalArgumentException](new Square(8, 2))
    assertThrows[IllegalArgumentException](new Square(1, 8))
  }

  test("rank and file have correct values") {
    val squareTest = new Square(0, 0)

    assertResult('1')(squareTest.rank)
    assertResult('a')(squareTest.file)

    val squareTest2 = new Square(4, 2)

    assertResult('5')(squareTest2.rank)
    assertResult('c')(squareTest2.file)

    val squareTest3 = new Square(7, 6)

    assertResult('8')(squareTest3.rank)
    assertResult('g')(squareTest3.file)
  }
}
