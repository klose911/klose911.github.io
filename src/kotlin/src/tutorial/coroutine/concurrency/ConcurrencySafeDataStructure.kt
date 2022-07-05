package tutorial.coroutine.concurrency

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import java.util.concurrent.atomic.AtomicInteger

var counter3 = AtomicInteger()

fun main() = runBlocking {
    withContext(Dispatchers.Default) {
        massiveRun {
            counter3.incrementAndGet()
        }
    }
    println("Counter = $counter3")
} 
