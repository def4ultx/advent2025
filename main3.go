package main

import (
	"fmt"
)

func main() {
	// fmt.Println(digit(12345))

	xs := []string{
		"3646122246265233144266436235253422621132626544356324544665325242262222212765227332424562134252555523",
		"7245465384344644334334444342433545356366344332495446345542464454541266464244443274454544425424873164",
		"2236252122122625244252232213543322314224432231222222232254232723224251242314122214122276232232242242",
		"6422222157312424653221232214722212222221222542522312242522223215412252622233222125123213231352428222",
		"5442253214354122234335455412232233424454239863362144311545224542243331245555523345523325255631352535",
		"4344142444124445544444484344324232434554224624442432554214348344444432242472242246642334134331516623",
		"7453643126653263862225322424476275744474755647441484416343344424445769444633852332346533273234424534",
		"5535335555549452745545533556554445345341457675534455695554442635335563453332535655456495344535453455",
		"3313343445332242323112332323352335323142343323233233423313442215345334352342533232435223533334323324",
		"5343122434654415724443254434443342245254318222524323424222262262351433613432354285434344316342262232",

		// "985467",
	}

	for _, x := range xs {
		fmt.Println(calculate(x))
	}
}

func removeK(input string) int {
	digit := 12

	for c := 0; c<digit;c++ {
		
	}


(* # # Old version with simpler logic but less efficient (O(K * N)).
# def max_power_general(line: List[int], nb_digits: int = 12) -> int:
#     digits: List[int] = []
#     position = 0
#     for i in range(nb_digits - 1, -1, -1):
#         if i > 0:
#             max_in_line = max(line[position:-i])
#         else:
#             max_in_line = max(line[position:])
#         digits.append(max_in_line)
#         position = line.index(max_in_line, position) + 1
#     return int("".join(map(str, digits))) *)


}

func calculate(input string) int {
	const c = 12

	l := len(input)
	xs := make([]int, l)
	for i := 0; i < l; i++ {
		xs[i] = int(input[l-i-1] - '0')
	}

	memo := [c][]int{}
	for i := 0; i < c; i++ {
		memo[i] = make([]int, l)
	}

	memo[0][0] = xs[0]
	for i := 1; i < l; i++ {
		memo[0][i] = max(memo[0][i-1], xs[i])
	}

	// fmt.Println(xs)
	// fmt.Println(memo[0])

	// fmt.Println("start")

	for i := 1; i < c; i++ {
		// fmt.Println(memo[i-1])
		for j := i; j < l; j++ {

			x1 := memo[i][j-1]

			xx := xs[j] * pow(10, digit(memo[i-1][j-1]))

			x2 := memo[i-1][j-1]

			// fmt.Printf("%d,%d - x1:%d, x2:%d xx:%d x:%d\n", i, j, x1, x2+xx, xx, xs[j])

			memo[i][j] = max(x1, x2+xx)
		}
	}

	// for i := 0; i < c; i++ {
	// 	fmt.Println(memo[i])
	// }

	return memo[c-1][l-1]
}

func digit(n int) int {
	d := 0
	for n > 0 {
		n /= 10
		d++
	}
	return d
}

func pow(base, expo int) int {
	b := 1
	for i := 0; i < expo; i++ {
		b *= base
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
