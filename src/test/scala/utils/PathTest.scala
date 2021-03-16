package com.mlobo
package utils

import org.scalatest.funsuite.AnyFunSuite

class PathTest extends AnyFunSuite {
  test("Path constructs properly") {
    assert(Path("").listedPath equals List())

    val p1 = Path("/")
    assert(p1.listedPath.equals(List()) && p1.base == "/")

    val p2 = Path("/test")
    assert(p2.listedPath.equals(List("test")) && p2.base == "/")

    val p3 = Path("/test/t2")
    assert(p3.listedPath.equals(List("test", "t2")) && p3.base == "/")

    val p4 = Path("test/t2")
    assert(p4.listedPath.equals(List("test", "t2")) && p4.base == ".")
  }

  test("path.join") {
    val p1 = Path("/test").join("coiso")
    assert(p1.base == "/" && p1.listedPath.equals(List("test", "coiso")))
  }

  test("path.toString") {
    assert(Path("/test/coiso").toString == "/test/coiso")
    assert(Path("test/coiso").toString == "./test/coiso")
    assert(Path("").toString == ".")
  }

  test("test.absolutePath") {
    assert(
      Path("/coiso/../test/test1").getAbsolutePath.toString == "/test/test1"
    )

    assert(
      Path("/coiso/../test/../test1").getAbsolutePath.toString == "/test1"
    )

    assert(
      Path("/coiso/test/../../test1").getAbsolutePath.toString == "/test1"
    )

    assert(
      Path(
        "/coiso/test/./test1"
      ).getAbsolutePath.toString == "/coiso/test/test1"
    )
  }
}
