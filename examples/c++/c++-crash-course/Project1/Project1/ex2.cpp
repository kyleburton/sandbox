#include <cstdio>
#include <memory>
#include <iostream>
#include <random>
#include <cstdint>
// #include <locale>
// #include <codecvt>
// NB: formatter is c++20
// #include <format>

constexpr int isqrt(int nn) {
	int ii = 1;
	while (ii * ii < nn) ++ii;
	return  ii - (ii * ii != nn);
}


struct HolmesIV {
	bool is_sentient;
	int sense_of_humor_rating;
};

void print_holmes(const char* name,  HolmesIV& ss) {
	printf("HolmesIV[%s]{is_sentient=%s; sense_of_humor_rating=%d}\n", name, ss.is_sentient ? "true ":"false", ss.sense_of_humor_rating);
}


void make_sentient(HolmesIV*);
void make_sentient(HolmesIV&);

void make_sentient(HolmesIV* mike) {
	if (nullptr == mike) {
		return;
	}
	mike->is_sentient = true;
}

void make_sentient(HolmesIV& mike) {
	mike.is_sentient = true;
}

/* cl ex2.cpp /fsanitize=addres /Zi
HolmesIV& not_dinkum(){
	HolmesIV mike;
	return mike;
}
*/

namespace Creature {
	struct Jabberwock {
		void* tugley_wood;
		int is_galumphing;
	};
}

struct Foundation {
	const char* founder;
	Foundation()
	: founder(nullptr) {
		std::cout << "Foundation coming online ..." << std::endl;
	}

	~Foundation() {
		const char* founder = "*unfounded*";
		if (this->founder != nullptr) {
			founder = this->founder;
		}
		std::cout << "Foundation[" << founder << "] collapsing ..." << std::endl;
	}
};

/*  NB: fomatter is c++20
template <>
struct std::formatter<Foundation> {
	constexpr auto parse(std::format_parse_context& ctx) {
		return / * * /;
	}

	auto format(const Foundation& obj, std::format_context& ctx) const {
		return std::format_to(ctx.out(), / * * /)
	}
};
*/

std::ostream& operator <<(std::ostream& o, const Foundation& foundation) {
	return o << "Foundation{founder=" << foundation.founder << "}";
}

void main1() {
	constexpr int xx = isqrt(1764);
	printf("%d\n", xx);

	HolmesIV mike1{};
	HolmesIV mike2{};

	print_holmes("mike", mike1);
	make_sentient(&mike1);
	print_holmes("mike1", mike1);

	print_holmes("mike2", mike2);
	make_sentient(mike2);
	print_holmes("mike2", mike2);

	HolmesIV* mike3 = nullptr;
	make_sentient(mike3);
	
	/*
	HolmesIV mike4;
	mike4 = not_dinkum();

	print_holmes("mike4", mike4);
	*/


	std::unique_ptr<Foundation> second_foundation{ new Foundation() };

	second_foundation->founder = "Wanda";
	// std::unique_ptr frees the owned memory via get_deleter(), more details to learn :)
	std::cout << "get_deleter()=" << &(second_foundation.get_deleter()) << std::endl;

}

struct Mutant {
	std::unique_ptr<Foundation> foundation;

	Mutant(std::unique_ptr<Foundation> foundation)
		: foundation(std::move(foundation)) {

	}

};

void main2() {
	std::unique_ptr<Foundation> second_foundation{ new Foundation() };
	Mutant the_mule{ std::move(second_foundation) };

	Foundation fdn;
	fdn.founder = "ramses";
	std::cout << "foundation=" <<  fdn << std::endl;
}

int step_function(int x) {
	if (x < 0) {
		return -1;
	}

	if (0 == x) {
		return 0;
	}

	return 1;
}

int main3() {

	/*
	for (int ii = 0; ii < 101; ++ii) {
		int xx = rand() % 2 == 0 ? -1 : 1;
		xx *= ii;
		std::cout << "xx=" << xx << "; " << step_function(xx) << std::endl;
	}
	*/
	int num1 = 42;
	int result1{ step_function(num1) };

	int num2 = 0;
	int result2 = step_function(num2);

	int num3 = 0;
	int result3 = step_function(num3);

	printf("num1: %d, step: %d\n", num1, result1);
	printf("num2: %d, step: %d\n", num2, result2);
	printf("num3: %d, step: %d\n", num3, result3);
	return 0;
}

int absolute_value(const int nn) {
	if (nn < 0) {
		return -1 * nn;
	}
	return nn;
}

void main4() {
	int my_num = -10;
	printf("absolute value of %d is %d\n", my_num, absolute_value(my_num));

	std::random_device rd;
	std::mt19937_64 gen(rd());
	std::uniform_int_distribution<> distr(-9999, 9999);

	for (int ii = 0; ii < 100; ++ii) {
		int nn = distr(gen);
		printf("absolute value of %d is %d\n", nn, absolute_value(nn));
	}
}

/*
std::string codepoint_to_utf8(char32_t codepoint) {
	std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> convert;
	return convert.to_bytes(&codepoint, &codepoint + 1);
}
*/
void main5() {
	int8_t nn = 3;
	int64_t mm = 37;
	int64_t gg = 1'000'000'000;
	printf("nn=%d; mm=%lld\n", nn, mm);
	printf("gg=%lld\n", gg);
	// https://stackoverflow.com/questions/1449805/how-to-format-a-number-using-comma-as-thousands-separator-in-c
	// setlocale(LC_NUMERIC, "");
	// printf("gg=%'lld\n", gg);

	printf("Yabba %x!\n", 3669732608);
	printf("Three are %u,%o leaves here.\n", 69, 69);

	// std::cout << "mug: " << U'\U0001F37A' << std::endl;
	// std::cout << "mug: " << codepoint_to_utf8(U'\U0001F37A') << std::endl;

	std::byte bb{0xFF};
	// std::cout << "bb=" << bb << std::endl;
	std::cout << "to_integer(bb)=" << std::to_integer<int32_t>(bb) << std::endl;

}

void main6() {
	unsigned long values[] = { 10, 50, 20, 40, 0 };
	size_t num_values = sizeof(values) / sizeof(unsigned long);
	size_t num_values2 = sizeof(values) / sizeof(values[0]);
	std::cout << "num_values=" << num_values << std::endl;
	std::cout << "num_values2=" << num_values2 << std::endl;
	for (auto val : values) {
		std::cout << "val=" << val << std::endl;
	}
}

enum class Race {
	Dinan,
	Teklan,
	Ivyn,
	Moiran,
	Camite,
	Julian,
	Aidan
};

const char* race_description(Race r) {
	switch (r) {
	case Race::Dinan:
		return "You work hard.";
		break;
	case Race::Teklan:
		return "You are very strong.";
		break;
	case Race::Ivyn:
		return "You are a great leader.";
		break;
	case Race::Moiran:
		return "My, how versatile you are!";
		break;
	case Race::Camite:
		return "You're incredibly helpful.";
		break;
	case Race::Julian:
		return "Anything you want!";
		break;
	case Race::Aidan:
		return "Wnat an enigma.";
		break;
	default:
		return "Error: unknown race!";
	}
}

void main7() {
	Race langobard_race = Race::Aidan;
	// std::cout << "langobard_race=" << std::to_integer<int32_t>(langobard_race) << std::endl;
	std::cout << "langobard_race=" << static_cast<int>(langobard_race) << std::endl;
	std::cout << "langobard_race=" << race_description(langobard_race) << std::endl;
}

class ClockOfTheLongNow {
	int year;
public:
	ClockOfTheLongNow() {
		year = 2019;
	}

	ClockOfTheLongNow(int yy) {
		year = 2019;
		set_year(yy);
	}

	void add_year() {
		year++;
	}
	bool set_year(int new_year) {
		if (new_year < 2019) {
			return false;
		}
		year = new_year;
		return true;
	}
	int get_year() {
		return year;
	}
};

struct PodStruct {
	uint64_t a;
	char b[256];
	bool c;
};

void print_pod(const PodStruct& pod) {
	std::cout << "PodStruct{a=" << pod.a << ";b=" << pod.b << ";" << (pod.c ? "true" : "false") << "}" << std::endl;
}

void main8() {
	ClockOfTheLongNow clock;
	printf("Year: %d\n", clock.get_year());
	if (!clock.set_year(2018)) {
		clock.set_year(2029);
	}
	clock.add_year();
	printf("Year: %d\n", clock.get_year());

	ClockOfTheLongNow clock2{ 2023 };
	printf("clock2.get_year(): %d\n", clock2.get_year());
	clock2.add_year();
	printf("clock2.get_year(): %d\n", clock2.get_year());

	PodStruct p1{ 42, "thing", true};
	print_pod(p1);
	PodStruct p2{ 42, true };
	print_pod(p2);
	// "true" ends up filling in the a field, and beomes a 1
	PodStruct p3{ true };
	print_pod(p3);
	// PodStruct p4{ "blue" };
	// print_pod(p4);
}

enum class Operation {
	Add,
	Subtract,
	Multiply,
	Divide
};

struct Calculator {
	Calculator(Operation op) {
		operation = op;
	}
	Operation operation;

	int calculate(int xx, int yy) {
		switch (operation) {
		case Operation::Add:
			return xx + yy;
			break;
		case Operation::Subtract:
			return xx - yy;
			break;
		case Operation::Multiply:
			return xx * yy;
			break;
		case Operation::Divide:
			return xx / yy;
			break;
		}
	}

	const char* opname() const {
		switch (operation) {
		case Operation::Add:
			return "add";
			break;
		case Operation::Subtract:
			return "subtract";
			break;
		case Operation::Multiply:
			return "multiply";
			break;
		case Operation::Divide:
			return "divide";
			break;
		}
	}

};

std::ostream& operator <<(std::ostream& o, const Calculator& obj) {
	return o << "Calculator{operation=" << obj.opname() << "}";
}


void main9() {
	Calculator c1{ Operation::Add };
	Calculator c2 = { Operation::Subtract };
	Calculator c3(Operation::Multiply);
	Calculator calculators[] = { c1, c2, c3, Calculator(Operation::Divide) };

	int xx = 99;
	int yy = 32;

	for (int ii = 0; ii < (sizeof(calculators) / sizeof(calculators[0])); ++ii) {
		Calculator cc = calculators[ii];
		std::cout << "ii=" << ii << ";" << cc << ".calculate(" << xx << "," << yy << ")=" << cc.calculate(xx, yy) << std::endl;
	}
	

}

int main () {
	// main1();
	// main2();
	// main3();
	// main4();
	// main5();
	// main6();
	// main7();
	// main8();
	main9();
	return 0;
}