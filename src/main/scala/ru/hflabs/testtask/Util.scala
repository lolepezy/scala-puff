package ru.hflabs.testtask

object Util {

  def timed[R](tag: String)(f: () => R): R = {
    val begin = System.currentTimeMillis()
    try {
      return f()
    } finally {
      val end = System.currentTimeMillis()
      println(tag + ", duration=" + (end - begin) + "ms")
    }
  }

}