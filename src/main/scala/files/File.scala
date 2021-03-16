package com.mlobo
package files

class File(override val parentPath: String, override val name: String)
    extends DirEntry(parentPath, name) {}
