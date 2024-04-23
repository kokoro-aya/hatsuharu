package moe.irony.resil.ext

import moe.irony.resil.Lib

class TestImport extends munit.FunSuite:
  test("export test") {
    val imported = Lib().someFunc()
    assertEquals(imported, 3)
  }