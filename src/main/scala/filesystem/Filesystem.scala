package com.mlobo
package filesystem

import commands.Command

import java.util.Scanner

object Filesystem extends App {
  var state = State.clean //vars is only for
  val scanner = new Scanner(System.in)

  while (true) {
    state.show()
    val input = scanner.nextLine()
    state = Command.from(input)(state)
  }
}
