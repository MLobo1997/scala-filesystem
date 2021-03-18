package com.mlobo
package utils

import org.scalatest.funsuite.AnyFunSuite

class PathTest extends AnyFunSuite {
  test("Path constructs properly") {
    assert(Path("").listedPath equals List())

    val p1 = Path("/")
    assert(p1.listedPath.equals(List()) && p1.isRoot)

    val p2 = Path("/test")
    assert(p2.listedPath.equals(List("test")) && p2.isRoot)

    val p3 = Path("/test/t2")
    assert(p3.listedPath.equals(List("test", "t2")) && p3.isRoot)

    val p4 = Path("test/t2")
    assert(p4.listedPath.equals(List("test", "t2")) && !p4.isRoot)
  }

  test("path.join") {
    val p1 = Path("/test").join("coiso")
    assert(p1.isRoot && p1.listedPath.equals(List("test", "coiso")))

    val p2 = Path("/test").join("")
    assert(p2.isRoot && p2.listedPath.equals(List("test")))

    val p3 = Path("/test").join("/coiso")
    assert(p3.isRoot && p3.listedPath.equals(List("coiso")))
  }

  test("path.toString") {
    assert(Path("/test/coiso").toString == "/test/coiso")
    assert(Path("test/coiso").toString == "test/coiso")
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
