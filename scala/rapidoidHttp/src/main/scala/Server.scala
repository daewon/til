import org.rapidoid.http.Req
import org.rapidoid.http.fast.{On, ReqHandler}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Server extends App {
  //  req -> Jobs.schedule(() -> {
  //    req.response().content("OK").done()
  //  }, 1, TimeUnit.SECONDS)

  On.get("/").json(new ReqHandler {
    override def handle(req: Req): AnyRef = {
      req.response().content(new String(req.body())).done()
    }
  })

  def handle(): ReqHandler = new ReqHandler {
    override def handle(req: Req): AnyRef = {
      val reqAsync = req.async()
      Future {
        reqAsync.response().content(req.toString).done
      }
    }
  }

  On.post("/").json(handle)
}
