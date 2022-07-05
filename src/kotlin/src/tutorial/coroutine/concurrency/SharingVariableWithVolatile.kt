package tutorial.coroutine.concurrency

import kotlinx.coroutines.*
import kotlin.system.measureTimeMillis

@Volatile
var counter2 = 0

fun main() = runBlocking {
    withContext(Dispatchers.Default) {
        massiveRun {
            counter2++
        }
    }
    println("Counter = $counter2")
}
