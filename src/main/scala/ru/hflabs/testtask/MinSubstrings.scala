package ru.hflabs.testtask
import scala.io.Source

object MinSubstrings extends App {
  if (args.length != 2)
    throw new Exception("Launch syntax is <application> <source file> <output file>");
  val source = args(0);
  val output = args(1);

  val lines = Source.fromFile(source, "cp1251").getLines().toArray;
}