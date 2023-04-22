include "other.file";

need
	- std::list::*;
	- std::io::print;
	- std::str;


proc factorial(int n) -> int {
	? n == 0 {
		1
	} : {
		n * factorial(n-1)
	}
}

proc max(int.. arr) -> int {
	arr -> reduce acc, elem {
		? acc > elem {
			 acc
		} : {
			elem
		}
	}
}

proc first(int.. arr) -> int {
	arr @ 0
}

proc main(str.. args) -> int {
	print(str::from_int(factorial(5)));

	int.. arr = [1,5,2,3];

	let msg = "Maximum element in array " + str::from_arr(arr) + " " + max(arr);
	print(msg);

	0
}
