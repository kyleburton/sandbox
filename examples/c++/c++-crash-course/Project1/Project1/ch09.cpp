#include <cstdio>
#include <string.h>

template<typename T>
constexpr T sum1(T x) {
	return x;
}

template<typename T, typename... Args >
constexpr T sum1(T x, Args... args) {
	return x + sum1(args...);;
}

template <typename... T>
constexpr auto sum2(T... args) {
	return (... + args);
}

int ch08_main();

auto make_char_counter(char ch) {
	return [ch](const char* str) -> int {
		const char* p = str;
		int acc{ 0 };
		while (*p) {
			if (*p == ch) ++acc;
			++p;
		}
		return acc;
	};
}

template <typename Fn, typename In, typename Out>
constexpr Out fold(Fn function, In* input, size_t length, Out initial) {
	Out result{ initial };
	for (size_t ii = 0; ii < length; ++ii) {
		result = function(input[ii], result);
	}
	return result;
}

#define STATIC_ARAY_LEN(things) (sizeof(things) / sizeof(things[0]))
void count_argv_chars(int argc, char** argv, int (&counts)[26]) {
	int result = 0;
	for (int ii = 0; ii < 26; ++ii) {
		counts[ii] = 0;
	}
	for (int ii = 0; ii < argc; ++ii) {
		for (int jj = 0; jj < strlen(argv[ii]); ++jj) {
			char ch = argv[ii][jj];
			if (ch >= 'a' && ch <= 'z') {
				int idx = ch - 'a';
				counts[idx]++;
				continue;
			}
			if (ch >= 'A' && ch <= 'Z') {
				int idx = ch - 'A';
				counts[idx]++;
				continue;
			}
		}
	}
}

template <typename Fn, typename In>
constexpr bool all(Fn function, In* input, size_t length) {
	for (size_t ii = 0; ii < length; ++ii) {
		if (!function(input[ii])) {
			return false;
		}
	}
	return true;
}

int main(int argc, char** argv) {
	printf("sum1 is: %d\n", sum1(2, 4, 6, 8, 10, 12));
	printf("sum2 is: %d\n", sum2(2, 4, 6, 8, 10, 12));

	const char* string = "xyzzy, a phony named xzylophone";
	auto count_x = make_char_counter('x');
	auto count_y = make_char_counter('y');
	auto count_z = make_char_counter('z');
	printf("num x's in %s = %d\n", string, count_x(string));
	printf("num y's in %s = %d\n", string, count_y(string));
	printf("num z's in %s = %d\n", string, count_z(string));

	auto squarer = [](int x) constexpr { return x * x; };
	printf("squarer(32)=%d\n", squarer(32));

	int data[]{100, 200, 300, 400, 500};
	size_t data_len = STATIC_ARAY_LEN(data);
	auto sum = fold([](auto elt, auto acc) { return elt + acc; }, data, data_len, 0);
	auto max = fold([](auto elt, auto acc) { return elt > acc ? elt : acc; }, data, data_len, 0);
	auto min = fold([](auto elt, auto acc) { return elt < acc ? elt : acc; }, data, data_len, data[0]);
	auto count_gt_200 = fold([](auto elt, auto acc) { return elt > 200 ? acc + 1 : acc; }, data, data_len, 0);
	printf("sum:          %d\n", sum);
	printf("max:          %d\n", max);
	printf("min:          %d\n", min);
	printf("count_gt_200: %d\n", count_gt_200);

	int hist[26]{};
	int sum_lengths = fold([](auto elt, auto acc) { return acc + strlen(elt);  }, argv, argc, 0);
	for (int ii = 0; ii < argc; ++ii) {
		printf("argv[%02d]='%s'\n", ii, argv[ii]);
	}
	printf("sum_lengths=%d\n", sum_lengths);
	count_argv_chars(argc, argv, hist);
	for (int ii = 0; ii < 26; ++ii) {
		printf("%c: ", ('a'+ii));
		for (int jj = 0; jj < hist[ii]; ++jj) {
			printf("*");
		}
		printf("\n");
	}

	// reuse data from above
	auto all_gt100 = all([](auto elt) { return elt > 100; }, data, data_len);
	if (all_gt100) {
		printf("all elements ARE greater than 100\n");
	}
	else {
		printf("all elements NOT greater than 100\n");
	}

	return 0;
}