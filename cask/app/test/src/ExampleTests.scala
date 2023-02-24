package app
import io.undertow.Undertow

import utest._

object ExampleTests extends TestSuite {
    def withServer[T](example: cask.main.Main)(f: String => T): T = {
        val server = Undertow.builder
            .addHttpListener(8081, "localhost")
            .setHandler(example.defaultHandler)
            .build
        server.start()
        val res = 
            try f("http://localhost:8081")
            finally server.stop()
        res
    }

    val tests = Tests {
        test("MinimalApplication") - withServer(MinimalApplication) { host =>
            val success = requests.get(host)

            success.text() ==> "Hello World!"
            success.statusCode ==> 200
        }
    }
}