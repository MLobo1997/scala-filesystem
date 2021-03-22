package com.mlobo
package files

import utils.Path

class File(
    override val parentPath: Path,
    override val name: String,
    val content: String = ""
) extends DirEntry(parentPath, name) {
  def setContent(newContent: String): File =
    new File(parentPath, name, newContent)
}
