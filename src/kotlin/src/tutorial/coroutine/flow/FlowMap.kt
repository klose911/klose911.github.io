package tutorial.coroutine.flow

import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.runBlocking

suspend fun performRequest(request: Int): String {
    delay(1000) // 模仿长时间运行的异步工作
    return "response $request"
}

fun main() = runBlocking {
    (1..3).asFlow() // 一个请求流
        .map { request -> performRequest(request) }
        .collect { response -> println(response) }
}