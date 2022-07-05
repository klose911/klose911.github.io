package main

import (
	"fmt"
	"math/cmplx"
)

var (
	ToBe   bool       = false
	MaxInt uint64     = 1<<64 - 1
	z      complex128 = cmplx.Sqrt(-5 + 12i)
)

func main() {
	const f = "%T(%v)\n"
	//bool(false)
	fmt.Printf(f, ToBe, ToBe)
	//uint64(18446744073709551615)
	fmt.Printf(f, MaxInt, MaxInt)
	//complex128((2+3i))
	fmt.Printf(f, z, z)
}
