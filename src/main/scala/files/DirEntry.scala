package com.mlobo
package files

import utils.Path
abstract class DirEntry(val parentPath: Path, val name: String) {
  def fullpath: Path = parentPath.join(name)
}
