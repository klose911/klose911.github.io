package tutorial.coroutine.flow

import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeoutOrNull

fun foo5(): Flow<Int> = flow {
    for (i in 1..3) {
        delay(100)
        println("Emitting $i")
        emit(i)
    }
}

fun main() = runBlocking {
    withTimeoutOrNull(350) { // 在 350 毫秒后超时
        foo5().collect { value -> println(value) }
    }
    println("Done")
}