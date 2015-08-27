import com.example.{MovementResult, Control}
import org.scalatest._

class ControlSpec extends FlatSpec with Matchers with BeforeAndAfter {

  before {

  }

  behavior of "a Control North"

  it should "LIMIT it self when at top" in {
    val control: Control = Control(Array(Array(1, 2, 3)))
    val north: MovementResult.Value = control.goNorth()
    assert(north == MovementResult.LIMIT)
  }

  it should "return OK when top is lower number" in {
    val control: Control = Control(1, 0,
                                  Array(Array(0, 0, 0), Array(1, 1, 1)))
    assert(control.getCurrentNumber == 1)
    val north: MovementResult.Value = control.goNorth()
    assert(north == MovementResult.OK)
    assert(control.getCurrentNumber == 0)
    assert(control.getMarks().size == 2)
  }

  it should "return NOT_OK when top is higher number" in {
    val control: Control = Control(1, 0,
                                  Array(Array(1, 1, 1), Array(0, 0, 0)))
    assert(control.getCurrentNumber == 0)
    val north: MovementResult.Value = control.goNorth()
    assert(north == MovementResult.NOT_OK)
    assert(control.getCurrentNumber == 0)
  }

  behavior of "a Control South"

  it should "LIMIT it self when at bottom" in {
    val control: Control = Control(Array(Array(1, 2, 3)))
    val south: MovementResult.Value = control.goSouth()
    assert(south == MovementResult.LIMIT)
  }

  it should "return OK when bottom is lower number" in {
    val control: Control = Control(0, 0,
      Array(Array(1, 1, 1), Array(0, 0, 0)))
    assert(control.getCurrentNumber == 1)
    val south: MovementResult.Value = control.goSouth()
    assert(south == MovementResult.OK)
    assert(control.getCurrentNumber == 0)
    assert(control.getMarks().size == 2)
  }

  it should "return NOT_OK when bottom is higher number" in {
    val control: Control = Control(0, 0,
      Array(Array(0, 0, 0), Array(1, 1, 1)))
    assert(control.getCurrentNumber == 0)
    val south: MovementResult.Value = control.goSouth()
    assert(south == MovementResult.NOT_OK)
    assert(control.getCurrentNumber == 0)
  }

  behavior of "a Control West"

  it should "LIMIT it self when at left most" in {
    val control: Control = Control(Array(Array(1, 2, 3)))
    val west: MovementResult.Value = control.goWest()
    assert(west == MovementResult.LIMIT)
  }

  it should "return OK when bottom is lower number" in {
    val control: Control = Control(0, 1,
      Array(Array(1, 2, 3)))
    assert(control.getCurrentNumber == 1)
    val north: MovementResult.Value = control.goWest()
    assert(north == MovementResult.OK)
    assert(control.getCurrentNumber == 0)
    assert(control.getMarks().size == 2)
  }

  it should "return NOT_OK when bottom is higher number" in {
    val control: Control = Control(0, 1,
      Array(Array(1, 2, 3)))
    assert(control.getCurrentNumber == 0)
    val north: MovementResult.Value = control.goWest()
    assert(north == MovementResult.NOT_OK)
    assert(control.getCurrentNumber == 0)
  }
}
