package org.lox

import org.scalatest._
import flatspec._
import org.scalatest.matchers.should.Matchers._

class ScannerTest extends AnyFlatSpec {
  "True" should "be equal to false" in {
    true should be (true)
  }
}
