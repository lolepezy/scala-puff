package ru.cxtest

import ru.circumflex._, core._, web._, freemarker._
import java.util.Date

class Main extends Router {
  val log = new Logger("ru.cxtest")

  'currentDate := new Date

  get("/test") = "I'm fine, thanks!"
  get("/") = ftl("index.ftl")

}
