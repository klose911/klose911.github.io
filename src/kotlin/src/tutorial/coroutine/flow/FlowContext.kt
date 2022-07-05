package tutorial.coroutine.flow

import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.runBlocking

fun log(msg: String) = println("[${Thread.currentThread().name}] $msg")

fun foo6(): Flow<Int> = flow {
    log("Started foo flow")
    for (i in 1..3) {
        emit(i)
    }
}

fun main() = runBlocking {
    foo6().collect { value -> log("Collected $value") }
}
