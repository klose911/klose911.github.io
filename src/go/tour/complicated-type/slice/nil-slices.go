package main

import "fmt"

func main() {
	var z []int
	//[] 0 0
	fmt.Println(z, len(z), cap(z))
	//nil!
	if z == nil {
		fmt.Println("nil!")
	}
}
