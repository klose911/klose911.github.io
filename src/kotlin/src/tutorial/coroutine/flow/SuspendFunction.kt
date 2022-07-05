package tutorial.coroutine.flow

import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking

suspend fun foo2(): List<Int> {
    delay(1000) // 假装我们在这里做了一些异步的事情
    return listOf(1, 2, 3)
}

fun main() = runBlocking<Unit> {
    foo2().forEach { value -> println(value) }
}
