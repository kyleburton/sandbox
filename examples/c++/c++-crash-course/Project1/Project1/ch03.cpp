#include <cstdio>

#define STATIC_ARRAY_LEN(aname) (sizeof(aname) / sizeof(aname[0]))

void print_poitner_address(void* addr) {
	/*
	unsigned long long b1 = (0xFFFF000000000000 & (unsigned long long)addr) >> 48;
	unsigned long long b2 = (0x0000FFFF00000000 & (unsigned long long)addr) >> 32;
	unsigned long long b3 = (0x00000000FFFF0000 & (unsigned long long)addr) >> 16;
	unsigned long long b4 = (0x000000000000FFFF & (unsigned long long)addr) >>  0;
	*/
	unsigned int b1 = 0xFFFF & (unsigned int)((unsigned long long)addr >> 48);
	unsigned int b2 = 0xFFFF & (unsigned int)((unsigned long long)addr >> 32);
	unsigned int b3 = 0xFFFF & (unsigned int)((unsigned long long)addr >> 16);
	unsigned int b4 = 0xFFFF & (unsigned int)((unsigned long long)addr >>  0);

	printf("addr{%04X %04X %04X %04X}", b1, b2, b3, b4);
}

void ch03_main1() {
	int gettysburg{};
	printf("gettysburg: %d\n", gettysburg);
	int* gettysburg_address = &gettysburg;
	printf("&gettysburg: 0x%p\n", gettysburg_address);
	printf("print_poitner_address: ");
	print_poitner_address(gettysburg_address);
	printf("\n");
}

struct College {
	char name[256];
};

void print_name(College* cc) {
	printf("%s College\n", cc->name);
}

void print_names(College* ccs, size_t n_ccs) {
	for ( size_t ii = 0; ii < n_ccs; ++ii) {
		printf("%s College\n", ccs[ii].name);
	}
}
void ch03_main2() {
	College colleges[] = { "Magdalen", "Nuffield", "Kellog" };
	// print_name(colleges);
	print_names(colleges, STATIC_ARRAY_LEN(colleges));

	auto the_answer{ 42 };
}


// 3-6
char read_from(char* elts, size_t n_elts, size_t idx) {
	if (idx < 0 || idx >= n_elts) {
		return '\0';
	}
	return elts[idx];
}

void write_to(char* elts, size_t n_elts, size_t idx, char ch) {
	if (idx < 0 || idx >= n_elts) {
		return;
	}
	elts[idx] = ch;
}

struct Element {
	Element* next{};
	Element* prev{};
	Element() {
		this->prefix[0] = '\0';
		this->prefix[1] = '\0';
		this->prefix[2] = '\0';
		this->operating_number = (short)- 1;
	}

	Element(char ch1, char ch2, short operating_number) : prefix{ ch1, ch2, '\0'}, operating_number{operating_number} {}

	void insert_after(Element* elt) {
		/**
            a->b
			a.insert_after(c)
			a->c->b
        */
		elt->next = next;
		if (this->next) {
			this->prev = elt;
		}

		elt->prev = this;
		next = elt;
	}
	void insert_before(Element* elt) {
		/**
			a->b
			a.insert_before(c)
			c->a->b
		*/
		elt->next = this;
		elt->prev = this->prev;
		if (this->prev) {
			this->prev->next = elt;
		}
		this->prev = elt;
	}

	Element* tail() {
		for (Element* cursor = this; cursor; cursor = cursor->next) {
			if (cursor->next == nullptr) {
				return cursor;
			}
		}
		return nullptr;
	}

	void print() const {
		for (auto cursor = this; cursor; cursor = cursor->next) {
			printf("Elt{%s-%d}", cursor->prefix, cursor->operating_number);
			if (cursor->next) {
				printf("->");
			}
		}
		printf("\n");
	}

	void print_reverse() {
		for (auto cursor = this->tail(); cursor; cursor = cursor->prev) {
			printf("Elt{%s-%d}", cursor->prefix, cursor->operating_number);
			if (cursor->prev) {
				printf("->");
			}
		}
		printf("\n");
	}

	char prefix[3];
	short operating_number;
	
};

void ch03_main3() {
	char lower[] = "abc?e";
	char upper[] = "ABC?E";
	char* upper_ptr = upper;
	char* lower_ptr = lower;
	printf("lower=%s\nupper=%s\n", lower, upper);
	write_to(lower, STATIC_ARRAY_LEN(lower), 3, 'd');
	write_to(upper, STATIC_ARRAY_LEN(upper), 3, 'D');

	printf("lower=%s\nupper=%s\n", lower, upper);

	write_to(lower, STATIC_ARRAY_LEN(lower), 99, 'd');
	write_to(upper, STATIC_ARRAY_LEN(upper), 99, 'D');

	printf("lower=%s\nupper=%s\n", lower, upper);


	Element trooper1{ 'A', 'A', 421 }, trooper2{ 'B', 'B', 2187 }, trooper3{ 'C', 'C', 137};
	// trooper1.insert_after(&trooper2);
	trooper2.insert_before(&trooper1);
	trooper1.print();

	trooper2.insert_after(&trooper3);
	trooper1.print();

	Element trooper4{'D', 'D', 298};
	trooper3.insert_before(&trooper4);

	trooper1.print();
	trooper1.print_reverse();
}

void ex_3_5() {
	auto original = 100;
	auto& original_ref = original;
	printf("original     =%d\n", original);
	printf("original_ref =%d\n", original_ref);

	auto new_val = 300;
	original_ref = new_val;
	printf("original     =%d\n", original);
	printf("new_val      =%d\n", new_val);
	printf("original_ref =%d\n", original_ref);
}

int ch03_main() {
	// ch03_main1();
	// ch03_main2();
	// ch03_main3();
	ex_3_5();

	return 0;
}