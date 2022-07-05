package tutorial.coroutine.flow

import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.transform
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    (1..3).asFlow() // 一个请求流
        .transform { request ->
            emit("Making request $request")
            emit(performRequest(request))
        }
        .collect { response -> println(response) }
}