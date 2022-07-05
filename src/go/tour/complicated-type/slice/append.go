package main

import "fmt"

func main() {
	var a []int
	//a len=0 cap=0 []
	printSlice("a", a)

	// append works on nil slices.
	a = append(a, 0)
	//a len=1 cap=2 [0]
	printSlice("a", a)

	// the slice grows as needed.
	a = append(a, 1)
	//a len=2 cap=2 [0 1]
	printSlice("a", a)

	// we can add more than one element at a time.
	a = append(a, 2, 3, 4)
	//a len=5 cap=8 [0 1 2 3 4]
	printSlice("a", a)
}

func printSlice(s string, x []int) {
	fmt.Printf("%s len=%d cap=%d %v\n",
		s, len(x), cap(x), x)
}
