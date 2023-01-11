package webui

import zhttp.http._

object WebUiApp {

 def apply(): Http[Any, Nothing, Request, Response] =
   Http.collect[Request] {
     // GET /greet/:name
     case Method.GET -> !! / "greet" / name => Response.text(s"Hello $name!")
     // GET /resp_json/
     case Method.GET -> !! / "resp_json" => Response.json("""{"greetings": "Hello World!"}""")
   }

}
