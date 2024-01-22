#include <gtest/gtest.h>
#include "ch10.hpp"

TEST(HelloTest, BasicAssertions) {
	EXPECT_STRNE("hello", "world");
	EXPECT_EQ(7*6, 42);
}

TEST(AutoBrakeTest, InitialCarSpeedIsZero) {
	MockServiceBus bus{};
	AutoBrake auto_brake{ bus };
	ASSERT_FLOAT_EQ(0, auto_brake.get_speed_mps());
}

struct MyTestFixture : ::testing::Test {};

TEST_F(MyTestFixture, MyTestA) {
// 	
}