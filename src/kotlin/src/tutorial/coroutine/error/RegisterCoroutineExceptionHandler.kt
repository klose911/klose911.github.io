package tutorial.coroutine.error

import kotlinx.coroutines.*

fun main() = runBlocking {
    val handler = CoroutineExceptionHandler { _, exception -> println("Caught $exception") }

    val job = GlobalScope.launch(handler) {
        throw AssertionError()
    }

    val deferred = GlobalScope.async(handler) {
        throw ArithmeticException() // 没有打印任何东西，依赖用户去调用 deferred.await()
    }
    joinAll(job, deferred)
} 
