#include <stdexcept>
#include <functional>

#define CATCH_CONFIG_MAIN
#include "catch_amalgamated.hpp"
#include "ch10.hpp"


constexpr void assert_that(bool statement, const char* message) {
	if (!statement) throw std::runtime_error{ message };
}

template<typename T> 
struct AutoBrake_v1 {
	AutoBrake_v1(const T& publish) 
		: publish{ publish }
		, collision_threshold_s{ 5.0 }
		, speed_mps{ 0.0 }
	{ }
	void observe(const SpeedUpdate & evt) { 
		speed_mps = evt.velocity_mps;
	}

	void observe(const CarDetected& evt) { 
		const auto relative_velocity_mps = speed_mps - evt.velocity_mps;
		const auto time_to_collision_s = evt.distance_m / relative_velocity_mps;
		if (time_to_collision_s > 0 && time_to_collision_s <= collision_threshold_s) {
			publish(BrakeCommand {time_to_collision_s});
		}
	}

	void set_collision_threshold(double x) {
		if (x < 1) {
			throw std::exception{ "Error: collision_threshold may not be less than 1.0" };
		}
		collision_threshold_s = x;
	}
	double get_collision_threshold_s() const {
		return collision_threshold_s;
	}
	double get_speed_mps() const {
		return speed_mps;
	}

private:
	double collision_threshold_s;
	double speed_mps;
	const T& publish;
};

////////////////////////////////////////////////////////////////////////////////
// Tests for V1
void initial_speed_is_zero_v1() {
	AutoBrake_v1 auto_brake{ [](const BrakeCommand&) {} };
	assert_that(auto_brake.get_speed_mps() == 0L, "Expected initial speed to be 0");
}

void initial_sensitivity_is_five_v1() {
	AutoBrake_v1 auto_brake{ [](const BrakeCommand&) {} };
	assert_that(auto_brake.get_collision_threshold_s() == 5L, "Expected sensitivity to be 5");
}

void sensitivity_must_be_greater_than_one_v1() {
	AutoBrake_v1 auto_brake{ [](const BrakeCommand&) {} };
	try {
		auto_brake.set_collision_threshold(-1.0);
	}
	catch (const std::exception&) {
		return;
	}
	assert_that(false, "unexpectedly able to set a negative (-1.0) collision_threshold!");
}

void speed_is_saved_v1() {
	AutoBrake_v1 auto_brake{ [](const BrakeCommand&) {} };
	auto_brake.observe(SpeedUpdate{ 100L });
	assert_that(100L == auto_brake.get_speed_mps(), "spped not saved to 100");
	auto_brake.observe(SpeedUpdate{ 50L });
	assert_that(50L == auto_brake.get_speed_mps(), "spped not saved to 50");
	auto_brake.observe(SpeedUpdate{ 0L });
	assert_that(0L == auto_brake.get_speed_mps(), "spped not saved to 0");
}

void alerts_when_crash_imminent_v1() {
	int brake_commands_published{};
	AutoBrake_v1 auto_brake{ [&brake_commands_published](const BrakeCommand&) { ++brake_commands_published;  } };
	auto_brake.set_collision_threshold(10L);
	auto_brake.observe(SpeedUpdate{ 100L });
	auto_brake.observe(CarDetected{ 100L, 0L });
	assert_that(1 == brake_commands_published, "expected one brake command to be published, zero were published");
}

void no_alert_when_crash_not_imminent_v1() {
	int brake_commands_published{};
	AutoBrake_v1 auto_brake{ [&brake_commands_published](const BrakeCommand&) { ++brake_commands_published;  } };
	auto_brake.set_collision_threshold(10L);
	auto_brake.observe(SpeedUpdate{ 100L });
	auto_brake.observe(CarDetected{ 25L, 100L });
	assert_that(0 == brake_commands_published, "expected no brake commands to be published, some were published!");
}


////////////////////////////////////////////////////////////////////////////////
// Test for V2

TEST_CASE("AutoBrake") {
	MockServiceBus bus{};
	AutoBrake auto_brake{ bus };

	SECTION("initial car speed is zero") {
		REQUIRE(auto_brake.get_speed_mps() == 0);
	}

	SECTION("initial sensitivity is five") {
		// REQUIRE(auto_brake.get_collision_threshold() == 5L);
		REQUIRE(auto_brake.get_collision_threshold_s() == Catch::Approx(5L));
	}

	SECTION("sensitivity must always be greater than 1") {
		// REQUIRE_THROWS(auto_brake.set_collision_threshold_s(0.5L));
		// REQUIRE_THROWS_AS(auto_brake.set_collision_threshold_s(0.5L), std::exception);
		// "Error: collision_threshold may not be less than 1.0"
		// REQUIRE_THROWS_WITH(auto_brake.set_collision_threshold_s(0.5L), Catch::Matchers::Contains("collision_threshold") && Catch::Matchers::Contains("may not be less than"));
		REQUIRE_THROWS_WITH(auto_brake.set_collision_threshold_s(0.5L), "Collision threshold cannot be less than 1");
	}

	SECTION("speed is saved when updated") {
		bus.speed_update_callback(SpeedUpdate{ 100L });
		REQUIRE(100L == auto_brake.get_speed_mps());
		bus.speed_update_callback(SpeedUpdate{ 50L });
		REQUIRE(50L == auto_brake.get_speed_mps());
		bus.speed_update_callback(SpeedUpdate{ 0L });
		REQUIRE(0L == auto_brake.get_speed_mps());
	}

	SECTION("no alert when a crash is not imminent") {
		auto_brake.set_collision_threshold_s(2L);
		bus.speed_update_callback(SpeedUpdate{ 100L });
		bus.car_detected_callback(CarDetected{ 1000L, 50L });
		REQUIRE(bus.commands_published == 0);
	}

	SECTION("an alert is fired when a crash is imminent") {
		auto_brake.set_collision_threshold_s(10L);
		bus.speed_update_callback(SpeedUpdate{ 100L });
		bus.car_detected_callback(CarDetected{ 100L, 0L });
		REQUIRE(bus.commands_published == 1);
		REQUIRE(bus.last_command.time_to_collision_s == 1L);
	}

}

////////////////////////////////////////////////////////////////////////////////

bool run_test(void(*unit_test)(), const char* name) {
	try {
		unit_test();
		printf("[+] PASS | %s\n", name);
		return true;
	}
	catch (const std::exception& e) {
		printf("[-] FAIL | %s | '%s'\n", name, e.what());
		return false;
	}

}

using TestFn = void(*)(void);

int main (int argc, char** argv) {
	// assert_that(1 + 2 > 2, "Something is wrong, math doesn't check out!");
	// assert_that(24 == 42, "This assertion will generate an exception.");
	std::pair<TestFn, const char*> tests[]{
		{ initial_speed_is_zero_v1,                "v1: Initial speed is zero"},
		{ initial_sensitivity_is_five_v1,          "v1: Initial sensitivity is five"},
		{ sensitivity_must_be_greater_than_one_v1, "v1: Sensitivty should always be positive"},
		{ speed_is_saved_v1,                       "v1: speed is saved"},
		{ alerts_when_crash_imminent_v1,           "v1: alerts when crash imminent"},
		{ no_alert_when_crash_not_imminent_v1,     "v1: no alert when a crash is not imminent"},

		// v2 tests
        // migrated test to Check2		
        // { initial_speed_is_zero,                   "v2: Initial speed is zero"},
        // { initial_sensitivity_is_five,             "v2: Initial sensitivity is five"},
		// { sensitivity_must_be_greater_than_1,      "v2: Sensitivty should always be positive"},
		// { speed_is_saved,                          "v2: speed is saved"},
		// { no_alert_when_crash_not_imminent,        "v2: no alert when a crash is not imminent"},
        // { alert_when_imminent,                     "v2: alerts when collision imminent"},

	};
	long num_tests_run{}, num_tests_passed{}, num_tests_failed{};
	for (auto pair : tests) {
		num_tests_run++;
		if (run_test(pair.first, pair.second)) {
			num_tests_passed++;
		}
		else {
			num_tests_failed++;
		}
	}
	double pct_passed = 100.0 * (num_tests_passed / num_tests_run);
	double pct_failed = 100.0 * (num_tests_failed / num_tests_run);
	printf("% 4d tests executed\n", num_tests_run);
	printf("% 4d tests passed : %3.1f\n", num_tests_passed, pct_passed);
	printf("% 4d tests failed : %3.1f\n", num_tests_failed, pct_failed);
	if (num_tests_failed != 0) {
		printf("\nFAILED\n\n");
		return 1;
	}

	printf("all tests passed\n");

	printf("////////////////////////////////////////////////////////////////////////////////\n");
	printf("// Catch 2 tests STARTING\n");
	printf("\n");
	int result = Catch::Session().run(argc, argv);
	printf("// Catch 2 tests COMPLETED\n");
	printf("////////////////////////////////////////////////////////////////////////////////\n");
	return result;
}