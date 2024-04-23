package moe.irony.resil

import moe.irony.resil.utils.IdentifierGenerator

class TestIdentifierGenerator extends munit.FunSuite:
  test("20") {
    val ig3 = IdentifierGenerator(3)
    val ig4 = IdentifierGenerator(4)

    for (i <- 0 to 19) {
      ig3.nextId
      ig4.nextId
    }

    assertEquals(ig3.nextId, "ACC")
    assertEquals(ig4.nextId, "AAA")
  }

  test("52") {
    val ig4 = IdentifierGenerator(4)
    val ig26 = IdentifierGenerator(26)

    for (i <- 0 to 51) {
      ig4.nextId
      ig26.nextId
    }

    assertEquals(ig4.nextId, "CAA")
    assertEquals(ig26.nextId, "BA")
  }