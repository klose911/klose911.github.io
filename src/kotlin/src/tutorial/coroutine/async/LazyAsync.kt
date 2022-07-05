package tutorial.coroutine.async

import kotlinx.coroutines.CoroutineStart
import kotlinx.coroutines.async
import kotlinx.coroutines.runBlocking
import kotlin.system.measureTimeMillis

fun main() = runBlocking {
    val time = measureTimeMillis {
        val one = async(start = CoroutineStart.LAZY) {
            doSomethingUsefulOne()
        }
        val two = async(start = CoroutineStart.LAZY) {
            doSomethingUsefulTwo()
        }
        // 执行一些计算
        one.start()    // 启动第一个
        two.start() // 启动第二个
        println("The answer is ${one.await() + two.await()}")
    }
    println("Completed in $time ms")
} 
