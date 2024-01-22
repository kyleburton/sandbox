#include <gtest/gtest.h>
#include "ch10.hpp"

struct AutoBrakeTest : ::testing::Test {
	MockServiceBus bus{};
	AutoBrake auto_brake{ bus };
};

TEST_F(AutoBrakeTest, InitialCarSpeedIsZero) {
	ASSERT_FLOAT_EQ(0, auto_brake.get_speed_mps());
}

TEST_F(AutoBrakeTest, InitialSensitivityIsFive) {
	ASSERT_DOUBLE_EQ(5, auto_brake.get_collision_threshold_s());
}

TEST_F(AutoBrakeTest, SensitivityGreaterThanOne) {
	ASSERT_ANY_THROW(auto_brake.set_collision_threshold_s(0.5L));
}


TEST_F(AutoBrakeTest, SpeedIsSaved) {
	bus.speed_update_callback(SpeedUpdate{ 100L });
	ASSERT_EQ(100, auto_brake.get_speed_mps());

	bus.speed_update_callback(SpeedUpdate{ 50L });
	ASSERT_EQ(50, auto_brake.get_speed_mps());

	bus.speed_update_callback(SpeedUpdate{ 0L });
	ASSERT_EQ(0, auto_brake.get_speed_mps());
}

TEST_F(AutoBrakeTest, NoAlertWhenCollisionNotImminent) {
	auto_brake.set_collision_threshold_s(2L);
	bus.speed_update_callback(SpeedUpdate{ 10L });
	bus.car_detected_callback(CarDetected{ 1000L, 50L });
	ASSERT_EQ(0, bus.commands_published);
}

TEST_F(AutoBrakeTest, AlertWhenCollisionImminent) {
	auto_brake.set_collision_threshold_s(10L);
	bus.speed_update_callback(SpeedUpdate{ 100L });
	bus.car_detected_callback(CarDetected{ 100L, 0L });
	ASSERT_EQ(1, bus.commands_published);
	ASSERT_DOUBLE_EQ(1L, bus.last_command.time_to_collision_s);
}