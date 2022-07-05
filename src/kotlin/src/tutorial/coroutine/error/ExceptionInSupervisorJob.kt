package tutorial.coroutine.error

import kotlinx.coroutines.CoroutineExceptionHandler
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.supervisorScope

fun main() = runBlocking {
    val handler = CoroutineExceptionHandler { _, exception -> println("Caught $exception") }
    supervisorScope {
	val child = launch(handler) {
	    println("Child throws an exception")
	    throw AssertionError()
	}
	println("Scope is completing")
    }
    println("Scope is completed")
}
