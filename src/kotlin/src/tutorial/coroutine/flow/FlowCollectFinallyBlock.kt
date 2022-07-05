package tutorial.coroutine.flow

import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.runBlocking

fun foo18(): Flow<Int> = (1..3).asFlow()

fun main() = runBlocking {
    try {
        foo18().collect { value -> println(value) }
    } finally {
        println("Done")
    }
}