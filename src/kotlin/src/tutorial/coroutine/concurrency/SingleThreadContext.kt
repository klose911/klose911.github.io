package tutorial.coroutine.concurrency

import kotlinx.coroutines.newSingleThreadContext
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext

val counterContext = newSingleThreadContext("CounterContext")
var counter4 = 0

fun main() = runBlocking {
//    withContext(Dispatchers.Default) {
        massiveRun {
            // 将每次自增限制在单线程上下文中
            withContext(counterContext) {
                counter4++
            }
        }
//    }
    println ("Counter = $counter4")
}

