package com.mlobo
package files

abstract class DirEntry(val parentPath: String, val name: String) {
  def fullpath: String =
    s"$parentPath${if (!parentPath.equals(Directory.SEPARATOR)) Directory.SEPARATOR
    else ""}$name"
}
