package tutorial.coroutine.error

import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.async
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    val job = GlobalScope.launch {
        println("Throwing exception from launch")
        throw IndexOutOfBoundsException() // 我们将在控制台打印 Thread.defaultUncaughtExceptionHandler
    }
    job.join()
    println("Joined failed job")

    val deferred = GlobalScope.async {
        println("Throwing exception from async")
        throw ArithmeticException() // 没有打印任何东西，依赖用户去调用等待
    }
    try {
        deferred.await()
        println("Unreached")
    } catch (e: ArithmeticException) {
        println("Caught ArithmeticException")
    }
} 
