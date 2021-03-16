package com.mlobo
package files

import utils.Path

class File(override val parentPath: Path, override val name: String)
    extends DirEntry(parentPath, name) {}
