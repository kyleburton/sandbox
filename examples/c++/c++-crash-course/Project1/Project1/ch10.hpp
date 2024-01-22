#pragma once

struct BrakeCommand {
	double time_to_collision_s;
};

struct SpeedUpdate {
	double velocity_mps;
};

struct CarDetected {
	double distance_m;
	double velocity_mps;
};

using SpeedUpdateCallback = std::function<void(const SpeedUpdate&)>;
using CarDetectedCallback = std::function<void(const CarDetected&)>;


struct IServiceBus {
	virtual ~IServiceBus() = default;
	virtual void publish(const BrakeCommand&) = 0;
	virtual void subscribe(SpeedUpdateCallback) = 0;
	virtual void subscribe(CarDetectedCallback) = 0;
};


////////////////////////////////////////////////////////////////////////////////
struct MockServiceBus : IServiceBus {
	void publish(const BrakeCommand& cmd) override {
		commands_published++;
		last_command = cmd;
	}
	void subscribe(SpeedUpdateCallback cb) override {
		speed_update_callback = cb;
	}
	void subscribe(CarDetectedCallback cb) override {
		car_detected_callback = cb;
	}

	BrakeCommand last_command{};
	int commands_published{};
	SpeedUpdateCallback speed_update_callback{};
	CarDetectedCallback car_detected_callback{};
};


////////////////////////////////////////////////////////////////////////////////
// AutoBrake V2
struct AutoBrake {
	AutoBrake(IServiceBus& bus)
		: collision_threshold_s{ 5 }
		, speed_mps{} {
		bus.subscribe([this](const SpeedUpdate& update) {
			speed_mps = update.velocity_mps;
			});
		bus.subscribe([this, &bus](const CarDetected& update) {
			const auto relative_velocity_mps = speed_mps - update.velocity_mps;
			const auto time_to_collision_s = update.distance_m / relative_velocity_mps;
			if (time_to_collision_s > 0
				&& time_to_collision_s <= collision_threshold_s) {
				bus.publish(BrakeCommand{ time_to_collision_s });
			}});
	}

	void set_collision_threshold_s(double tt) {
		if (tt < 1) {
			throw std::exception{ "Collision threshold cannot be less than 1" };
		}
		collision_threshold_s = tt;
	}
	double get_collision_threshold_s() {
		return collision_threshold_s;
	}
	double get_speed_mps() {
		return speed_mps;
	}
private:
	double collision_threshold_s;
	double speed_mps;
};
