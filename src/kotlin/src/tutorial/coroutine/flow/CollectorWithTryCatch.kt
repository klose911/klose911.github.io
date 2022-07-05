package tutorial.coroutine.flow

import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.runBlocking

fun foo12(): Flow<Int> = flow {
    for (i in 1..3) {
        println("Emitting $i")
        emit(i) // emit next value
    }
}

fun foo14(): Flow<String> =
    flow {
        for (i in 1..3) {
            println("Emitting $i")
            emit(i) // emit next value
        }
    }.map { value ->
        check(value <= 1) { "Crashed on $value" }
        "string $value"
    }


fun main() = runBlocking {
    try {
        foo12().collect { value ->
            println(value)
            check(value <= 1) { "Collected $value" }
        }
    } catch (e: Throwable) {
        println("Caught $e")
    }

    try {
        foo14().collect { value -> println(value) }
    } catch (e: Throwable) {
        println("Caught $e")
    }
}