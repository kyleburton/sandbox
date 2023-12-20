#include <cstdio>
#include <iostream>
#include <type_traits>

void carbon_thaw(const int& encased_solo) {
	std::cout << "carbon_thaw encased_solo=" << encased_solo << std::endl;
	auto& hibernation_sick_solo = const_cast<int&>(encased_solo);
	std::cout << "carbon_thaw hibernation_sick_solo=" << hibernation_sick_solo << std::endl;
	hibernation_sick_solo++;
	std::cout << "carbon_thaw hibernation_sick_solo=" << hibernation_sick_solo << std::endl;
}

short increment_as_short(void* target) {
	auto as_short = static_cast<short*>(target);
	*as_short = *as_short + 1;
	return *as_short;
}

template <typename To, typename From>
To narrow_cast(From value) {
	const auto converted = static_cast<To>(value);
	const auto backwards = static_cast<From>(converted);
	if (value != backwards) throw std::runtime_error{ "Narrowed!" };
	return converted;
}

template <typename T>
T mean(const T* values, size_t length) {
	static_assert(std::is_default_constructible<T>(), "Type must be default constructable.");
	static_assert(std::is_copy_constructible<T>(), "Type must be copy constructable.");
	static_assert(std::is_arithmetic<T>(), "Type must support addition and division.");
	static_assert(std::is_constructible<T, size_t>(), "Type must be constructible from size_t.");
	T result{};
	for (size_t ii{}; ii < length; ++ii) {
		result += values[ii];
	}
	return result / static_cast<T>(length);
}

void ch06_main1() {
	/* named conversion functions */
	const int encased_solo = 99;
	std::cout << "encased_solo=" << encased_solo << std::endl;
	carbon_thaw(encased_solo);
	std::cout << "encased_solo=" << encased_solo << std::endl;

	short beast{ 664 };
	std::cout << "beast=" << beast << std::endl;
	auto mark_of = increment_as_short(&beast);
	std::cout << "beast=" << beast << std::endl;

	// unsafe / undefined casts us reinterpret_cast
	// auto timer = reinterpret_cast<const unsigned long*>(0x1000);
	// printf("Timer is %lu\n", *timer);

	int perfect{ 496 };
	const auto perfect_short = narrow_cast<short>(perfect);
	printf("perfect_short: %d\n", perfect_short);
	try {
		int cyclic{ 142857 };
		const auto cyclic_short = narrow_cast<short>(cyclic);
		printf("cyclic_short: %d\n", cyclic_short);
	} 
	catch (const std::runtime_error& e) {
		printf("Exception: %s\n", e.what());
	}

	const double nums_d[]{ 1.0, 2.0, 3.0, 4.0 };
	// const auto result1 = mean<double>(nums_d, sizeof(nums_d) / sizeof(nums_d[0]));
	const auto result1 = mean(nums_d, sizeof(nums_d) / sizeof(nums_d[0]));
	printf("double: %f\n", result1);

	const float nums_f[]{ 1.0f, 2.0f, 3.0f, 4.0f };
	// const auto result2 = mean<float>(nums_f, sizeof(nums_f) / sizeof(nums_f[0]));
	const auto result2 = mean(nums_f, sizeof(nums_f) / sizeof(nums_f[0]));
	printf("float: %f\n", result2);

	const size_t nums_c[]{ 1, 2, 3, 4 };
	// const auto result3 = mean<size_t>(nums_c, sizeof(nums_c) / sizeof(nums_c[0]));
	const auto result3 = mean(nums_c, sizeof(nums_c) / sizeof(nums_c[0]));
	printf("size_t: %zu\n", result3);

	// fails with "error C2338: static_assert failed: 'Type must support addition and division.'"
	//const int* int_ptrs[3];
	//const int aa = 1;
	//const int bb = 3;
	//const int cc = 5;
	//int_ptrs[0] = &aa;
	//int_ptrs[1] = &bb;
	//int_ptrs[2] = &cc;
	//const auto result4 = mean(int_ptrs, sizeof(int_ptrs) / sizeof(int_ptrs[0]));
	//printf("char: %c\n", result4);
}


int main() {
	ch06_main1();
	return 0;
}