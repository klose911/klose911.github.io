package tutorial.coroutine.flow

import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.onEach
import kotlinx.coroutines.runBlocking

fun events(): Flow<Int> = (1..3).asFlow().onEach { delay(1000) }

fun main() = runBlocking {
    events()
        .onEach { event -> println("Event: $event") }
        .collect{} // <--- Collecting the flow waits
    println("Done")
}