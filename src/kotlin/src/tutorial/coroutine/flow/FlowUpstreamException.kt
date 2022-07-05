package tutorial.coroutine.flow

import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.onCompletion
import kotlinx.coroutines.runBlocking

fun foo20(): Flow<Int> = (1..3).asFlow()

fun main() = runBlocking {
    foo20()
        .onCompletion { cause -> println("Flow completed with $cause") }
        .collect { value ->
            check(value <= 1) { "Collected $value" }
            println(value)
        }
}