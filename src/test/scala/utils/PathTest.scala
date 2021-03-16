package com.mlobo
package utils

import org.scalatest.funsuite.AnyFunSuite

class PathTest extends AnyFunSuite {
  test("Path constructs properly") {
    assert(Path("").listedPath equals List())

    val p1 = Path("/")
    assert(p1.listedPath.equals(List()) && p1.basePath == '/')

    val p2 = Path("/test")
    assert(p2.listedPath.equals(List("test")) && p2.basePath == '/')

    val p3 = Path("/test/t2")
    assert(p3.listedPath.equals(List("test", "t2")) && p3.basePath == '/')

    val p4 = Path("test/t2")
    assert(p4.listedPath.equals(List("test", "t2")) && p4.basePath == '.')
  }
}
