package main

import "fmt"

func main() {
	var a [2]string
	a[0] = "Hello"
	a[1] = "World"
	// Hello World
	fmt.Println(a[0], a[1])
	//[Hello World]
	fmt.Println(a)
}
