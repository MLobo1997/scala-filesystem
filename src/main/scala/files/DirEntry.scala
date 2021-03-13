package com.mlobo
package files

abstract class DirEntry(val parentPath: String, val name: String) {
  def fullpath: String = s"$parentPath${Directory.SEPARATOR}$name"
}
