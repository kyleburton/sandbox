#include <cstdio>
#include <iostream>
#include <stdexcept>

/* unfortunately immer incurrs a lot of warnings w/Visual Studio, about loss of data/precision */
#pragma warning(disable: 4267)
#include <immer/vector.hpp>
#include <immer/algorithm.hpp>
#include <map>
#pragma warning(default: 4267)

std::ostream& operator<<(std::ostream& o, const immer::vector<long long>& v) {
	o << "[";
	immer::for_each(v, [&](const long long ii) mutable {
		o << ii << ",";
		});
	o << "]";

	return o;
}

void ch05_main1() {
	printf("trying out the persistent functional data structures library `immer`: https://github.com/arximboldi/immer\n");

	const auto v0 = immer::vector<long long>{};
	const auto v1 = v0.push_back(13);
	assert(v0.size() == 0 && v1.size() == 1 && v1[0] == 13);

	const auto v2 = v1.set(0, 42);
	assert(v1[0] == 13 && v2[0] == 42);

	auto fibs = immer::vector<long long>{1, 1};
	for (int ii = 1; ii < 999; ++ii) {
		long long aa = fibs[fibs.size() - 2];
		long long bb = fibs[fibs.size() - 1];
		if (aa > (std::numeric_limits<long long>::max() - bb)) {
	// 9223372036854775807; aa=4660046610375530309 + bb=7540113804746346429 puts us over the limit!
	//      9223372036854775807
    // aa = 4660046610375530309 
    // bb = 7540113804746346429
			std::cout << "fibs: hit max! std::numeric_limits<long long>::max()=" << std::numeric_limits<long long>::max() << "; aa=" << aa << " + bb=" << bb << " puts us over the limit!" << std::endl;
			break;
		}
		long long nextval = aa + bb;
		fibs = fibs.push_back(nextval);
	}

	std::cout << "fibs=" << fibs << std::endl;
}


struct FileLogger {
	void log_transfer(long from, long to, double amount) {
		printf("[file] %ld -> %ld: %f\n", from, to, amount);
	}
};
struct ConsoleLogger {
	void log_transfer(long from, long to, double amount) {
		printf("[console] %ld -> %ld: %f\n", from, to, amount);
	}
};

enum class LoggerType {
	Console,
	File
};

struct Bank {
	Bank()
	: type{ LoggerType::Console } {
	}

	void set_logger(LoggerType new_type) {
		type = new_type;
	}

	void make_transfer(long from, long to, double amount) {
		switch (type) {
		case LoggerType::Console:
				consoleLogger.log_transfer(from, to, amount);
				break;
		case LoggerType::File:
			fileLogger.log_transfer(from, to, amount);
			break;
		default:
			throw std::logic_error("Unknown logger type encountered.");
		}
	}

private:
	LoggerType type;

private:
	ConsoleLogger consoleLogger;
	FileLogger fileLogger;
};

void ch05_main2() {
	Bank bank;
	bank.make_transfer(1000, 2000, 49.95);
	bank.make_transfer(2000, 4000, 20.00);
	bank.set_logger(LoggerType::File);
	bank.make_transfer(3000, 2000, 75.00);
}

struct Logger2 {
	virtual ~Logger2() = default;
	virtual void log_transfer(long from, long to, double amount) = 0;
};

struct ConsoleLogger2 : Logger2 {
	virtual void log_transfer(long from, long to, double amount) {
		printf("[console2] %ld -> %ld: %f\n", from, to, amount);
	}
};

struct FileLogger2 : Logger2 {
	virtual void log_transfer(long from, long to, double amount) {
		printf("[file2] %ld -> %ld: %f\n", from, to, amount);
	}
};

struct Bank2 {
	Bank2(Logger2& logger)
		: logger{ logger } {
	}

	void make_transfer(long from, long to, double amount) {
		logger.log_transfer(from, to, amount);
	}


	Logger2& logger;
};

void ch05_main3() {
	ConsoleLogger2 logger{};
	Bank2 bank{ logger };
	bank.make_transfer(1000, 2000, 49.95);
	bank.make_transfer(2000, 4000, 20.00);
}

struct Bank3 {
	void set_logger(Logger2* new_logger) {
		logger = new_logger;
	}
	void make_transfer(long from, long to, double amount) {
		if (logger) {
			logger->log_transfer(from, to, amount);
		}
	}

	Logger2* logger;
};

class InsufficientFunds : std::logic_error {
public:
	InsufficientFunds(long account, double balance, double amount)
		: std::logic_error("insufficient funds")
		, account{ account }
		, balance{ balance }
		, amount{ amount } {
	}
	long account;
	double balance;
	double amount;
};

std::ostream& operator<<(std::ostream& o, const InsufficientFunds& e) {
	o << "InsufficientFunds{account=" << e.account << "; balance=" << e.balance << "; amount=" << e.amount << "}";
	return o;
}

class AccountNotFound : std::logic_error {
public:
	AccountNotFound(long account)
		: std::logic_error("insufficient funds")
		, account{ account } {
	}
	long account;
};

std::ostream& operator<<(std::ostream& o, const AccountNotFound& e) {
	o << "AccountNotFound{account=" << e.account << "}";
	return o;
}

class DuplicateAccount : std::logic_error {
public:
	DuplicateAccount(long account)
		: std::logic_error("duplicate account")
		, account{ account } {
	}
	long account;
};

std::ostream& operator<<(std::ostream& o, const DuplicateAccount& e) {
	o << "DuplicateAccount{account=" << e.account << "}";
	return o;
}

void ch05_main4() {
	ConsoleLogger2 logger{};
	FileLogger2 file_logger{};
	Bank3 bank;
	bank.set_logger(&logger);
	bank.make_transfer(1000, 2000, 49.95);
	bank.set_logger(&file_logger);
	bank.make_transfer(2000, 4000, 20.00);
}


////////////////////////////////////////////////////////////////////////////////


class AccountDatabase {
public:
	virtual void create_account(long account, double initial_balance) = 0;
	virtual double get_balance(long account) const = 0;
	virtual void set_balance(long account, double balance) = 0;
	virtual void make_transfer(long from, long to, double amount) {
		double bal = get_balance(from);
		if (bal < amount) {
			throw InsufficientFunds{from, bal, amount};
		}
		set_balance(from, bal - amount);
		set_balance(to, get_balance(to) + amount);
	}
	virtual void credit(long account, double amount) = 0;
	virtual void debit(long account, double amount) = 0;

	virtual void print(std::ostream& o) const {
		o << "AccountDatabase{}";
	}

};

class InMemoryAccountDatabase : public AccountDatabase {
public:	
	InMemoryAccountDatabase()
		: database{} {
	}

	virtual void create_account(long account, double initial_balance) override {
		auto result = database.find(account);
		if (result != database.end()) {
			throw DuplicateAccount(account);
		}

		database[account] = initial_balance;
	}

	virtual double get_balance(long account) const override {
		auto result = database.find(account);
		if (result == database.end()) {
			throw AccountNotFound(account);
		}
		return result->second;
	}

	virtual void set_balance(long account, double balance) override {
		auto result = database.find(account);
		if (result == database.end()) {
			throw AccountNotFound(account);
		}
		database[account] = balance;
	}

	virtual void debit(long account, double amount) override {
		auto result = database.find(account);
		if (result == database.end()) {
			throw AccountNotFound(account);
		}
		database[account] -= amount;
	}

	virtual void credit(long account, double amount) override {
		auto result = database.find(account);
		if (result == database.end()) {
			throw AccountNotFound(account);
		}
		database[account] += amount;
	}

	virtual void print(std::ostream& o) const override {
		o << "InMemoryAccountDatabase{";
		for (const auto& [account, balance] : database) {
			o << "[account=" << account << ",balance=" << balance << "],";
		}
		o << "}";
	}

private:
	std::map<long, double> database;
};

std::ostream& operator<<(std::ostream& o, const AccountDatabase& db) {
	db.print(o);
	return o;
}

std::ostream& operator<<(std::ostream& o, const AccountDatabase* db) {
	db->print(o);
	return o;
}

void ch05_main5() {
	AccountDatabase* db = new InMemoryAccountDatabase{};
	db->create_account(1000, 100.00);
	db->create_account(2000, 400.00);
	db->create_account(3000, 900.00);
	db->create_account(4000, 0.0);

	std::cout << "AccountDatabase" << std::endl;
	std::cout << db << std::endl;

	db->make_transfer(1000, 2000, 50.00);
	std::cout << db << std::endl;

	delete db;
}

void ch05_main() {
	// ch05_main1();
	// ch05_main2();
	// ch05_main3();
	// ch05_main4();
	ch05_main5();
}