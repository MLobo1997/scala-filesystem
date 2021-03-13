package com.mlobo
package filesystem

import commands.Command
import files.Directory

import java.util.Scanner

object Filesystem extends App {
  val root = Directory.ROOT
  var state = State.clean //vars is only for
  val scanner = new Scanner(System.in)

  while (true) {
    state.show()
    val input = scanner.nextLine()
    state = Command.from(input)(state)
  }
}
