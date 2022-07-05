package main

import "fmt"

func main() {
	sum := 1
	for sum < 1000 {
		sum += sum
	}
	//1024
	fmt.Println(sum)
}
