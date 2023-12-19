#include <cstdio>
#include <stdexcept>
#include <system_error>
#include <iostream>
#include <string>
#include <ctime>
#include <chrono>

extern int rat_things_power = 200;

void power_up_rat_thing(int nuclear_isotope) {
	rat_things_power = rat_things_power + nuclear_isotope;
	const auto waste_heat = rat_things_power * 20;
	if (waste_heat > 10000) {
		printf("Warning! Hot doggie!\n");
	}
}


int power_up_rat_thing2(int nuclear_isotope) {
	static thread_local int rat_things_power2 = 200;
	rat_things_power2 = rat_things_power2 + nuclear_isotope;
	const auto waste_heat = rat_things_power2 * 20;
	if (waste_heat > 10000) {
		printf("Warning! Hot doggie!\n");
	}
	printf(" rat_things_power2: %d\n", rat_things_power2);
	return rat_things_power2;
}

struct RatThing {
	static int rat_things_power;
	static void power_up_rat_thing(int nuclear_isotope) {
		rat_things_power = rat_things_power + nuclear_isotope;
		const auto waste_heat = rat_things_power * 20;
		if (waste_heat > 10000) {
			printf("Warning! Hot doggie!\n");
		}
		printf("Rat-thing power: %d\n", rat_things_power);
	}
};

int RatThing::rat_things_power = 200;

void ch04_main1() {

	//power_up_rat_thing(100);
	//printf("Rat-thing power: %d\n", rat_things_power);
	//power_up_rat_thing(500);
	//printf("Rat-thing power: %d\n", rat_things_power);
	
	RatThing::power_up_rat_thing(100);
	RatThing::power_up_rat_thing(500);

	int power{};
	power = power_up_rat_thing2(0);
	printf("Rat-thing2 power: %d\n", power);
	power = power_up_rat_thing2(100);
	printf("Rat-thing2 power: %d\n", power);
	power = power_up_rat_thing2(500);
	printf("Rat-thing2 power: %d\n", power);
}

struct Tracer {
	Tracer(const char* name) : name{ name } {
		printf("Tracer|%s constructed.\n", name);
	}
	~Tracer() {
		printf("Tracer|%s destructed.\n", name);
	}
	const char* const name;
};

static Tracer t1{ "static-t1" };
thread_local Tracer t2{ "thread-local-t2" };

void ch04_main2() {
	const auto t2_ptr = &t2;
	printf("A\n");
	Tracer t3{ "stack-auto-t3" };
	printf("B\n");
	const auto* t4 = new Tracer{ "heap-dynamic-t4" };
	printf("C\n");
	delete t4;
}

struct Groucho {
	void forget(int x) {
		if (x == 0xFACE) {
			throw std::runtime_error{"I'd be glad to make an exception."};
		}
		printf("Forgot 0x%x\n", x);
	}
};

void ch04_main3() {
	Groucho gg;
	try {
		gg.forget(0xC0DE);
		gg.forget(0xFACE);
		gg.forget(0xC0FFEE);
	}
	catch (std::runtime_error& ex) {
		printf("exception caught with message: %s\n", ex.what());
	}

	try {
		throw std::logic_error{ "It's not about who wrong It's not about who right" };
	}
	catch(const std::exception & ex) {
		// printf("caught std::exception[%s] ex=%s\n", typeid(ex).name(), ex.what());
		std::cout << "caught std::exception[" << typeid(ex).name() << "] '" << ex.what() << "'" << std::endl;
		// the throw statment rethrows the caught exception
		// throw;
	}
}

////////////////////////////////////////////////////////////////////////////////
struct HumptyDumpty {
	HumptyDumpty() : together{ true }  {
	}
	bool is_together_again() {
		return together;
	}

private:
	bool together{ true };
};
struct Result {
	HumptyDumpty hd;
	bool success;
};

Result make_humpty() {
	HumptyDumpty hd{};
	bool is_valid = true;
	return { hd, is_valid };
}

bool send_kings_horses_and_men() {
	auto [hd, success] = make_humpty();
	if (!success) { return false; }
	return true;
}


std::pair<HumptyDumpty, bool> send_kings_horses_and_men2() {
	HumptyDumpty hd{};
	return std::pair<HumptyDumpty, bool>(hd, true);
}

struct SimpleString {
	SimpleString(size_t max_size)
		: max_size{ max_size }
		, length{} {
		buffer = new char[max_size];
		buffer[0] = '\0';
	}

	// copy constructor
	SimpleString(const SimpleString& other) 
		: max_size{ other.max_size }
		, buffer{ new char[other.max_size] } 
		, length{ other.length } {
		strncpy_s(buffer, max_size, other.buffer, max_size);
	}

	// move constructur, move constructors must be marked noexcept or the compiler won't use them
	SimpleString(SimpleString&& other) noexcept
		: max_size { other.max_size }
		, buffer { other.buffer }
		, length{ other.length }  {
		other.length = 0;
		other.buffer = nullptr;
		other.max_size = 0;
		
	}

	SimpleString& operator=(const SimpleString& other) {
		if (this == &other) { 
			return *this;  
		}
		delete[] buffer;
		this->max_size = other.max_size;
		this->length = other.length;
		buffer = new char[this->max_size];
		strncpy_s(buffer, length, other.buffer, length);
		return *this;
	}

	// move assignment operator, mark as noexcept or the compiler won't use it
	SimpleString& operator=(SimpleString&& other) noexcept {
		if (this == &other) {
			return *this;
		}
		delete[] buffer;
		buffer = other.buffer;
		length = other.length;
		max_size = other.max_size;
		other.buffer = nullptr;
		other.length = 0;
		other.max_size = 0;
		return *this;
	}

	~SimpleString() {
		delete[] buffer;
		length = 0;
		buffer = nullptr;
	}

	void print(const char* tag) const {
		if (buffer == nullptr) {
			printf("%s<invalid>: *nullptr*\n", tag);
			return;
		}
		printf("%s: %s", tag, buffer);
	}

	bool append_line(const char* xx) {
		const auto xx_len = strlen(xx);
		if (xx_len + length + 2 > max_size) { return false; }
		strncpy_s(buffer + length, max_size - length, xx, max_size - length);
		length += xx_len;
		buffer[length++] = '\n';
		buffer[length] = '\0';
		return true;
	}

	// attempting to set a non-default to 'default' does trigger an error:
	//     is not a special member function or comparison operator which can be defaulted
	// void some_func2() = default;
    // attempting to set a non-defualt to delete does not generate an error or warning ...
	// void some_func() = delete;
private:
	size_t max_size;
	size_t length;
	char* buffer;
};


void ch04_main4() {
	if (send_kings_horses_and_men()) {
		printf("1| sending kings horses and men!\n");
	}
	else {
		printf("1| ...not sending.\n");
	}

	auto res = send_kings_horses_and_men2();
	if (res.second) {
		printf("2| sending kings horses and men!\n");
	}
	else {
		printf("2| ...not sending.\n");
	}


	SimpleString aa{ 50 };
	aa.append_line("We apologize for the");
	SimpleString aa_copy{ aa };
	aa.append_line("inconvienience.");
	aa_copy.append_line("incontinence.");
	aa.print("a");
	aa_copy.print("aa_copy");
}

/*
 
   value categories
   ----------------------------------------
   glvalue:  generalized lvalue
   prvalue:  pure rvalue
   xvalue:   expiring value
   lvalue:   a glvalue that isn't an xvalue
             kinda: any value that has a name
   rvalue:   a prvalue or an xvalue
             kinda: anything that isn't an lvalue

   lvalue reference    &
   rvalue reference    &&

  pg: 129 Compile-Generated Methods
  * destructor
  * copy constructor
  * move constructor
  * copy assignment operator
  * move assignment operator
  each of these can be set to 'default' or 'delete' to be explicit

  known as the "rule of five"  
*/

void ref_type(int& x) {
	printf("ref_type: lvalue reference x=%d\n", x);
}

void ref_type(int&& x) {
	printf("ref_type: rvalue reference x=%d\n", x);
}

void ch04_main5() {
	auto x = 1;
	ref_type(x);
	ref_type(std::move(x));
	// is it still valid to use x?
	// ref_type(x);
	ref_type(2);
	ref_type(x + 2);

	SimpleString a{ 50 };
	a.append_line("We apologize for the");
	SimpleString b{ 50 };
	b.append_line("last message");
	a.print("a");
	b.print("b");
	b = std::move(a);
    // ok, the compiler (IDE?) does warn about use of a moved object.
	// a.print("a");
	b.print("b");
}

struct TimerClass {
	TimerClass(const char* name) 
		: is_moved{ false }
		, start { std::chrono::system_clock::now() }
		, end { }
		, name{ name }  {
	}

	TimerClass(const TimerClass& other)
		: is_moved{ false }
		, start{ other.start }
		, end{ other.end }
		, name{ other.name }  {
	}

	TimerClass(TimerClass&& other) noexcept
		: is_moved{ false }
		, start{ other.start }
		, end{ other.end }
		, name{ other.name }  {
		other.is_moved = true;
	}

	TimerClass& operator=(const TimerClass& other) {
		this->start = other.start;
		this->end = other.end;
		this->name = other.name;
	}

	TimerClass& operator=(TimerClass&& other) noexcept {
		this->start = other.start;
		this->end = other.end;
		this->name = other.name;
		other.is_moved = true;
	}

	~TimerClass() {
		if (is_moved) {
			return;
		}
		char sbuff[32];
		char ebuff[32];
		end = std::chrono::system_clock::now();
		std::chrono::duration<double> elapsed = end - start;
		std::time_t start_time = std::chrono::system_clock::to_time_t(start);
		std::time_t end_time = std::chrono::system_clock::to_time_t(end);
		ctime_s(sbuff, 32, &start_time);
		ctime_s(ebuff, 32, &end_time);
		char* loc;
		loc = strchr(sbuff, '\n');
		if (loc) *loc = '\0';
		loc = strchr(ebuff, '\n');
		if (loc) *loc = '\0';
		auto nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(elapsed);
		// std::cout << "TimerClass{start=" << sbuff << ", end=" << ebuff << "} elapsed=" << elapsed.count() << std::endl;
		std::cout << "TimerClass{name=" << name << "; start=" << sbuff << ", end=" << ebuff << "} elapsed=" << nanoseconds.count() << std::endl;
	}

	void set_name(const char* name) {
		this->name = name;
	}

private:
	bool is_moved;
	std::chrono::system_clock::time_point start;
	std::chrono::system_clock::time_point end;
	const char* name;
};

void ch04_main6() {
	TimerClass t1{ "t1" };
	TimerClass t2{ "t2" };
	TimerClass t3{ std::move(t2) };
	t3.set_name("t3");
}

int main() {
	// ch04_main1();
	// ch04_main2();
	// ch04_main3();
	// ch04_main4();
	// ch04_main5();
	ch04_main6();
	return 0;
}