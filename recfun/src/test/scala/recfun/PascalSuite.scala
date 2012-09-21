package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("col=0,row=0") {
    assert(pascal(0,0) === 1)
  }
  
  test("col=0,row=1") {
    assert(pascal(0,1) === 1)
  }
  
  test("col=1,row=1") {
    assert(pascal(1,1) === 1)
  }
  
  test("col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("col=2,row=2") {
    assert(pascal(2,2) === 1)
  }
  
  test("col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  
  test("col=2,row=3") {
    assert(pascal(2,3) === 3)
  }
  
  test("col=3,row=3") {
    assert(pascal(3,3) === 1)
  }
  
  test("col=2,row=5") {
    assert(pascal(2,5) === 10)
  }
  
  test("col=-1,row=0 Out of bounds") {
    intercept[IllegalArgumentException] {
      pascal(-1,0)
    }
  }
  
  test("col=-1,row=-1 Out of bounds") {
    intercept[IllegalArgumentException] {
      pascal(-1,0)
    }
  }
  
  test("col=0,row=-1 Out of bounds") {
    intercept[IllegalArgumentException] {
      pascal(-1,0)
    }
  }
  
  test("col=5,row=2 Out of bounds") {
    intercept[IllegalArgumentException] {
      pascal(-1,0)
    }
  }
  
}
