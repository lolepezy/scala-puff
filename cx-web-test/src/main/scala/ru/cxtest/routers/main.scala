package ru.cxtest

import ru.circumflex._
import ru.circumflex.core._
import ru.circumflex.web._
import ru.circumflex.freemarker._
import java.util.Date
import ru.cxtest.model.BoobsPost

class Main extends Router {
  val log = new Logger("ru.cxtest")

  'currentDate := new Date

  get("/test") = "I'm fine, thanks!"
  get("/") = ftl("index.ftl")

  get("/boobs") = ftl("boobs.ftl", BoobsPost.getRecent(10))

}
