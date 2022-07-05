package main

import "fmt"

func main() {
	ch := make(chan int, 2)
	ch <- 1
	ch <- 2
	// 1
	fmt.Println(<-ch)
	// 2
	fmt.Println(<-ch)
}
