package main

import "fmt"

// counting
// done
// 9
// 8
// 7
// 6
// 5
// 4
// 3
// 2
// 1
// 0
func main() {
	fmt.Println("counting")

	for i := 0; i < 10; i++ {
		defer fmt.Println(i)
	}

	fmt.Println("done")
}
